open Utils
open Speclang
open Specelab
open Z3
open Z3.SMT
open Z3.Sort
open Z3.Expr
open Z3.Optimize
open Z3.Solver
open Z3.Symbol
open Z3.Datatype
open Z3.FuncDecl
open Z3.Boolean
open Z3.Arithmetic 
open Z3.Arithmetic.Integer
open Z3.Quantifier
module Solver = Z3.Solver
module OptSolver = Z3.Optimize
module Model = Z3.Model
module Symbol = Z3.Symbol
module Optimize = Z3.Optimize
module Int = Z3.Arithmetic.Integer
module Bool = Z3.Boolean
module Quantifier = Z3.Quantifier
module Expr = Z3.Expr
module Constructor = Z3.Datatype.Constructor
module VC = Vc

let mk_new_ctx () = 
  let cfg = [("model", "true"); ("proof", "false")] in
    mk_context cfg

let ctx = ref @@ mk_new_ctx ()
let solver = ref @@ mk_solver !ctx None 
let (cmap : (string,Expr.expr) Hashtbl.t) = Hashtbl.create 211
let (tmap : (Type.t,Sort.sort) Hashtbl.t) = Hashtbl.create 47
let (fmap : (string,FuncDecl.func_decl) Hashtbl.t) = Hashtbl.create 47

let fresh_bv_name = gen_name "bv" 
let fresh_bv () = Ident.create @@  fresh_bv_name ()

let reset () = 
  begin
    ctx := mk_new_ctx ();
    solver := mk_solver !ctx None;
    Hashtbl.clear cmap;
    Hashtbl.clear tmap;
    Hashtbl.clear fmap;
  end

let sort_of_typ typ = try Hashtbl.find tmap typ 
                      with Not_found ->
                        (printf "%s not found in tmap" 
                           (Type.to_string typ); raise Not_found)
let fun_of_str str = try Hashtbl.find fmap str
                      with Not_found ->
                        (printf "%s not found in fmap" str;
                         raise Not_found)
let const_of_name n = try Hashtbl.find cmap n 
                      with Not_found -> (printf "%s not found in cmap" n; 
                                         raise Not_found)
let const_of_id id = const_of_name @@ Ident.name id
let all_mkkey_funs () = Hashtbl.fold (fun name func acc -> 
                          if Str.string_match (Str.regexp "^mkkey_") name 0
                          then func::acc else acc) fmap []
(*
 * Z3 API for the current ctx
 *)
let sym s = Symbol.mk_string !ctx s
let mk_app f args = mk_app !ctx f args
let mk_int_sort () = Int.mk_sort !ctx
let mk_bool_sort () = Bool.mk_sort !ctx
let mk_numeral_i i = Int.mk_numeral_i !ctx i
let mk_uninterpreted_s s = mk_uninterpreted_s !ctx s
let mk_const_s str sort = Expr.mk_const_s !ctx str sort
let mk_constructor_s a b c d e = mk_constructor_s !ctx a b c d e
let mk_sort_s a b = mk_sort_s !ctx a b
let mk_func_decl_s name arg_sorts res_sort = 
  mk_func_decl_s !ctx name arg_sorts res_sort
let mk_and conjs = mk_and !ctx conjs
let mk_or conjs = mk_or !ctx conjs
let mk_eq e1 e2 = mk_eq !ctx e1 e2
let mk_gt e1 e2 = mk_gt !ctx e1 e2
let mk_lt e1 e2 = mk_lt !ctx e1 e2
let mk_not e = mk_not !ctx e
let mk_true () = mk_true !ctx
let mk_false () = mk_false !ctx
let mk_ite e1 e2 e3 = mk_ite !ctx e1 e2 e3
let _assert e = Solver.add !solver [e]
let _assert_all e = Solver.add !solver e
let check_sat () = Solver.check !solver []

let vis (e1,e2) = mk_app (fun_of_str "vis") [e1; e2]
let so (e1,e2) = mk_app (fun_of_str "so") [e1; e2]
let hb (e1,e2) = mk_app (fun_of_str "hb") [e1; e2]
let sameobj (e1,e2) = mk_app (fun_of_str "sameobj") [e1; e2]
let objtyp e = mk_app (fun_of_str "objtyp") [e]
let objid e = mk_app (fun_of_str "objid") [e]
let ssn e = mk_app (fun_of_str "ssn") [e]
let txn e = mk_app (fun_of_str "txn") [e]
let seqno e = mk_app (fun_of_str "seqno") [e]
let oper e = mk_app (fun_of_str "oper") [e]
let (@=>) e1 e2 = mk_implies !ctx e1 e2
let (@<=>) e1 e2 = mk_iff !ctx e1 e2
let (@&) e1 e2 = mk_and [e1; e2]
let (@|) e1 e2 = mk_or [e1; e2]
let (@=) e1 e2 = mk_eq e1 e2
let (@<) e1 e2 = mk_lt e1 e2
let (@>) e1 e2 = mk_gt e1 e2
let (@>=) e1 e2 = (e1 @> e2) @| (e1 @= e2)
let (@!=) e1 e2 = mk_not (e1 @= e2)
let (!@) e = mk_not e

let forall sorts f = 
  let n = List.length sorts in
  let names = List.tabulate n 
                (fun i -> sym @@ "a"^(string_of_int i)) in
  let vars = List.tabulate n 
               (fun i -> mk_bound !ctx (n-i-1) 
                           (List.nth sorts i)) in
  let body = f vars in
    mk_forall !ctx sorts names body None [] [] None None

let forallE1 f = 
  let s_Eff = Hashtbl.find tmap Type.eff in
  let sorts = [s_Eff] in
  let f' vars = match vars with 
    | [x] -> f x | _ -> failwith "Unexpected!" in
    forall sorts f' 

let forallE2 f = 
  let s_Eff = Hashtbl.find tmap Type.eff in
  let sorts = [s_Eff; s_Eff] in
  let f' vars = match vars with 
    | [x; y] -> f x y | _ -> failwith "Unexpected!!" in
    forall sorts f' 

let forallE3 f = 
  let s_Eff = Hashtbl.find tmap Type.eff in
  let sorts = [s_Eff; s_Eff; s_Eff] in
  let f' vars = match vars with 
    | [x; y; z] -> f x y z | _ -> failwith "Unexpected!!!" in
    forall sorts f' 

let forallE4 f = 
  let s_Eff = Hashtbl.find tmap Type.eff in
  let sorts = [s_Eff; s_Eff; s_Eff; s_Eff] in
  let f' vars = match vars with 
    | [w; x; y; z] -> f w x y z | _ -> failwith "Unexpected!!!" in
    forall sorts f' 

let declare_enum_type (ty:Type.t) (consts: Ident.t list) =
  let mk_cstr e = 
    let name = Ident.name e in 
      mk_constructor_s name (sym @@ "is"^name) [] [] [] in
  let cstrs = List.map mk_cstr consts in
  let s_ty = mk_sort_s (Type.to_string ty) cstrs in
  let s_consts = List.map (fun f -> mk_app f []) @@
                      Datatype.get_constructors s_ty in
  begin
    Hashtbl.add tmap ty s_ty;
    List.iter (fun (c,s_c) -> 
                 Hashtbl.add cmap (Ident.name c) s_c)
      (List.zip consts s_consts);
    printf "%s added\n" (Type.to_string ty);
  end

let declare_variant_type (ty:Type.t) (consts: Cons.t list) =
  let mk_cstr (Cons.T {name; recognizer}) = 
      mk_constructor_s (Ident.name name) 
        (sym @@ Ident.name recognizer) [] [] [] in
  let cstrs = List.map mk_cstr consts in
  let s_ty = mk_sort_s (Type.to_string ty) cstrs in
  let s_consts = List.map (fun f -> mk_app f []) @@
                      Datatype.get_constructors s_ty in
  begin
    Hashtbl.add tmap ty s_ty;
    List.iter (fun (Cons.T {name},s_c) -> 
                 Hashtbl.add cmap (Ident.name name) s_c)
      (List.zip consts s_consts);
    printf "%s added\n" (Type.to_string ty);
  end

let declare_types (ke,te) =
  begin
    Hashtbl.add tmap Type.Int (mk_int_sort ());
    Hashtbl.add tmap Type.Bool (mk_bool_sort ());
    Hashtbl.add tmap Type.String (mk_uninterpreted_s "Stringe");
    KE.iter (fun tyid kind -> 
               let tyname () = Type._of @@ Ident.name tyid in
                 match kind with
                   | Kind.Variant consts -> 
                       declare_variant_type (tyname ()) consts
                   | Kind.Enum consts -> 
                       declare_enum_type (tyname ()) consts
                   | Kind.Extendible consts ->
                       declare_enum_type (tyname ()) !consts
                   | _ -> ()) ke;
    (* If the type is not already mapped by tmap, map it to an 
     * uninterpreted sort (including option and list types) *)
    let rec add_if_unknown typ = let f = add_if_unknown in 
      match typ with | Type.Arrow (t1,t2) -> (f t1; f t2)
        | Type.Int | Type.Bool | Type.String | Type.Unit -> ()
        | _ -> if Hashtbl.mem tmap typ then () 
               else let sort_name = Str.strip_ws (Type.to_string typ) in
                    let _ = printf "%s added\n" sort_name in
                    let sort = mk_uninterpreted_s sort_name in
                      Hashtbl.add tmap typ sort in
      TE.iter (fun _ typ -> add_if_unknown typ) te;
  end

let declare_vars te = 
  let rec uncurry_arrow = function 
      Type.Arrow (t1, (Type.Arrow (_,_) as t2)) ->
        (fun (x,y) -> (t1::x,y)) (uncurry_arrow t2)
    | Type.Arrow (t1,t2) -> ([t1],t2)
    | _ -> failwith "uncurry_arrow called on a non-arrow type" in
  let declare_fun name typ = 
    let (arg_typs,res_typ) = uncurry_arrow typ in
    let (arg_sorts,res_sort) = (List.map sort_of_typ arg_typs, 
                                sort_of_typ res_typ) in
    let func_decl = mk_func_decl_s name arg_sorts res_sort in
    let _ = printf "%s being added\n" name in
      Hashtbl.add fmap name func_decl in
  let declare_const name typ = 
    let sort = sort_of_typ typ in
    let const_decl = mk_const_s name sort in
      Hashtbl.add cmap name const_decl in
    TE.iter (fun id typ -> let name = Ident.name id in 
               match typ with
                 | Type.Arrow _ -> declare_fun name typ
                 | Type.Unit -> ()
                 | _ -> declare_const name typ) te

let assert_axioms ke =
  let s_NOP = const_of_id (Cons.name Cons.nop) in 
  let s_ENOP = const_of_id (L.e_nop) in
  let s_ssn_nop = const_of_id (L.ssn_nop) in
  let s_txn_nop = const_of_id (L.txn_nop) in
  (* _ENOP is the only NOP effect *)
  let e_not_nop = forallE1 (fun a -> 
                              (oper(a) @= s_NOP) @<=> (a @= s_ENOP)) in
  (* ssn(_ENOP) = _nop_ssn *)
  let enop_ssn = ssn(s_ENOP) @= s_ssn_nop in
  (* txn(_ENOP) = _nop_txn *)
  let enop_txn = txn(s_ENOP) @= s_txn_nop in
  (* mkkey functions are bijections *)
  let mkkeys = all_mkkey_funs () in 
  let bijection f a b = ((mk_app f [a]) @= (mk_app f [b])) @=> (a @= b) in
  let mkkey_bijections = 
    List.map (fun mkkey_f -> 
                let sorts = match FuncDecl.get_domain mkkey_f with
                  | [dom] -> [dom; dom] 
                  | _ -> failwith "mkkey_bijections: Unexpected" in
                  forall sorts (function [a;b] -> bijection mkkey_f a b
                                  | _ -> failwith "Impossible!")) mkkeys in
  (* seq. nos are non-negative *)
  let seqno_pos = forallE1 (fun a -> seqno(a) @>= (mk_numeral_i 0)) in
  (* sameobj *)
  let sameobj_def = forallE2 
                      (fun a b -> 
                         sameobj(a,b) @<=> mk_and 
                                            [oper(a) @!= s_NOP; 
                                             oper(b) @!= s_NOP;
                                             objtyp(a) @= objtyp(b);
                                             objid(a) @= objid(b) ]) in
  (* so *)
  let so_def = forallE2 
                 (fun a b -> 
                    so(a,b) @<=> mk_and [oper(a) @!= s_NOP; 
                                         oper(b) @!= s_NOP;
                                         ssn(a) @= ssn(b); 
                                         seqno(a) @< seqno(b)]) in
  (* so is transitive *)
  let so_trans = forallE3 
                   (fun a b c -> 
                      (so(a,b) @& so(b,c)) @=> so(a,c)) in
  (* vis => sameobj *)
  let vis_sameobj = forallE2 
                      (fun a b -> vis(a,b) @=> sameobj(a,b)) in
  (* hb is (vis U so)+ *)
  let hb_def1 = forallE2
                  (fun a b -> 
                     (vis(a,b) @| so(a,b)) @=> hb(a,b)) in
  let hb_def2 = forallE3 
                  (fun a b c -> 
                     (hb(a,b) @& hb(b,c)) @=> hb(a,c)) in
  (* hb is acyclic *)
  let hb_acyclic = forallE1 (fun a -> mk_not @@ hb(a,a)) in
  let qasns1 = [sameobj_def; so_def; so_trans; vis_sameobj;
                e_not_nop; hb_def1; hb_def2; hb_acyclic; seqno_pos] in
  let qasns2 = mkkey_bijections in
  let asns1 = List.map (fun q -> expr_of_quantifier q) @@ 
                List.concat [qasns1; qasns2] in
  let asns2 = [enop_ssn; enop_txn] in
    _assert_all @@ asns1@asns2

let assert_contracts () = 
  let gt = const_of_name "Tweet_Get" in
  let f a b c d = 
    mk_and [oper(d) @= gt; so(a,b); 
            vis(b,c); so(c,d); sameobj(a,d)] @=> vis(a,d) in
  let asn = expr_of_quantifier @@ forallE4 f in
    _assert asn

(*
 * Encoding
 *)
module P = Predicate
module S = SymbolicVal

let rec doIt_sv sv = 
  let open S in 
  let f = doIt_sv in 
    match sv with 
      | Var id -> const_of_id id
      | App (id,svs) -> mk_app (fun_of_str @@ Ident.name id)
                          (List.map f svs) 
      | Eq (v1,v2) -> (f v1) @= (f v2)
      | Lt (v1,v2) -> (f v1) @< (f v2)
      | Gt (v1,v2) -> (f v1) @> (f v2)
      | Not v -> mk_not @@ f v
      | And vs -> mk_and @@ List.map f vs
      | Or vs -> mk_or @@ List.map f vs
      | ConstInt i -> mk_numeral_i i
      | ConstBool true -> mk_true ()
      | ConstBool false -> mk_false ()
      | ITE (v1,v2,v3) -> mk_ite (f v1) (f v2) (f v3)
      | _ -> failwith "doIt_sv: Unimpl."

let rec doIt_pred p = match p with
  | P.BoolExpr v -> doIt_sv v
  | P.If (t1,t2) -> (doIt_pred t1) @=> (doIt_pred t2)
  | P.Iff (t1,t2) -> (doIt_pred t1) @<=> (doIt_pred t2)
  | P.Forall (ty,f) -> expr_of_quantifier @@
      forall [sort_of_typ ty] 
        (fun exprs -> match exprs with 
           | [expr] -> 
               let bv = fresh_bv () in
               let _ = Hashtbl.add cmap (Ident.name bv) expr in
               let p = doIt_pred @@ f bv in
               let _ = Hashtbl.remove cmap (Ident.name bv) in
                 p
           | _ -> failwith "doIt_pred: Unexpected")
  | _ -> failwith "doIt_pred: Unimpl."

let declare_pred name p =
  let s_pred = mk_const_s name (sort_of_typ Type.Bool) in
  let e_pred = doIt_pred p in
    begin
      Hashtbl.add cmap name s_pred;
      _assert @@ s_pred @<=> e_pred 
    end

let assert_const name = 
  let s_pred = Hashtbl.find cmap name in
  _assert s_pred

let assert_prog prog = 
  _assert_all @@ List.map doIt_pred prog

let assert_neg_const name = 
  let s_pred = Hashtbl.find cmap name in
  _assert (mk_not s_pred)

let discharge (txn_id, vc) = 
  let open VC in
    begin
      declare_types (vc.kbinds, vc.tbinds);
      declare_vars vc.tbinds;
      assert_axioms vc.kbinds;
      assert_contracts ();
      assert_prog vc.prog;
      declare_pred "pre" vc.pre;
      declare_pred "post" vc.post;
      assert_const "pre";
      assert_neg_const "post";
      Printf.printf "*****  CONTEXT ******\n";
      print_string @@ Solver.to_string !solver;
      print_string "(check-sat)\n";
      print_string "(get-model)\n";
      Printf.printf "\n*********************\n";
      flush_all ();
      check_sat ();
    end

let doIt vcs = 
  if not (Log.open_ "z3.log") then
    failwith "Log couldn't be opened."
  else
    let res = discharge (List.hd vcs) in
    begin
      (match res with 
        | SATISFIABLE -> printf "SAT\n"
        | UNSATISFIABLE -> 
            printf "%s verified!\n" 
              (Ident.name @@ fst @@ List.hd vcs)
        | UNKNOWN -> printf "UNKNOWN\n");
      Printf.printf "Disposing...\n";
      Gc.full_major ();
    end
