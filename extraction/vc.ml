open Speclang
open Specelab
module SV = SymbolicVal
module P = Predicate

(* Sequential Verification Condition *)
type seq_t = TE.t * Predicate.t list * Predicate.t

(* Verification Condition *)
type t = {kbinds:KE.t; tbinds: TE.t; 
          (* program : a set of constraints (on effects) 
           * describing a (write) transaction's operational 
           * behaviour. *)
          prog: Predicate.t list;
          (* invariant : a set of constraints describing the
           * operational behaviour of a (read) tranasction, along 
           * with a constraint that must hold under such 
           * behaviour so that the transaction always returns true.*)
          (* inv: Predicate.t; *)
          (* pre-condition: a constraint that "projects" 
           * the invariant on the pre-state (i.e., set of effects
           * not including those constrained by prog) *)
          pre: Predicate.t; 
          (* post-condition: a constraint that "projects" 
           * the invariant on the post-state (i.e., set of all 
           * effects, including those constrained by prog) *)
          post: Predicate.t}

let printf = Printf.printf

let print_seq (te,antePs,conseqP) = 
  begin
    printf "bindings {\n";
    TE.print te;
    printf "} in\n";
    List.iter (fun p -> 
                 Printf.printf "  /\\   %s\n" 
                   (P.to_string p)) antePs;
    print_string "=>";
    Printf.printf "\t%s\n" (P.to_string conseqP);
  end

let print {kbinds; tbinds; prog; pre; post} =
  begin
    printf "--------- Concurrent VC -------\n";
    printf "\027[38;5;4mbindings {\027[0m\n";
    printf "  \027[38;5;4mkinds\027[0m\n";
    KE.print kbinds;
    printf "  \027[38;5;4mtypes\027[0m\n";
    TE.print tbinds;
    printf "\027[38;5;4m} \027[0m\n";
    printf "\027[38;5;4mprog: \027[0m\n";
    List.iter (fun p -> 
                 Printf.printf "    /\\   %s\n" 
                   (P.to_string p)) prog;
    printf "\027[38;5;4mpre: \027[0m\n";
    (*List.iter (fun p -> 
                 Printf.printf "    /\\   %s\n" 
                   (P.to_string p)) (pre::antePs);*)
    Printf.printf "    %s\n" (P.to_string pre);
    (*print_string "  =>";
    Printf.printf "    %s\n" (P.to_string inv);*)
    printf "\027[38;5;4mpost: \027[0m\n";
    (*List.iter (fun p -> 
                 Printf.printf "    /\\   %s\n" 
                   (P.to_string p)) (post::antePs);*)
    Printf.printf "    %s\n" (P.to_string post);
    (*print_string "  =>";
    Printf.printf "    %s\n" (P.to_string inv);*)
  end
