open Microblog_app_runtime
open Uuid 
open String
open Lwt
open Cohttp
open Cohttp_lwt_unix

 let createTable tableName = 
    let stringURI = concat "" ["http://localhost:3000";"/createTable"; "/"; tableName] in
    let headers = Header.add (Header.init ()) "content-type" "application/json" in
    let bodyString = "{}" in
    let post = Lwt_main.run (Client.post ~headers ~body:(Cohttp_lwt_body.of_string bodyString) (Uri.of_string stringURI) >>= fun (response, body) ->
                        match response with
                        | { Cohttp.Response.status = `OK; _ } -> body |> Cohttp_lwt_body.to_string
                        | { Cohttp.Response.status; _ } -> body |> Cohttp_lwt_body.to_string) in
    ()

 let notifyAboutTable tableName port =     
    let stringURI = concat "" ["http://localhost:";(string_of_int port);"/notifyAboutTable"; "/"; tableName] in
    let headers = Header.add (Header.init ()) "content-type" "application/json" in
    let bodyString = "{}" in
    let post = Lwt_main.run (Client.post ~headers ~body:(Cohttp_lwt_body.of_string bodyString) (Uri.of_string stringURI) >>= fun (response, body) ->
                        match response with
                        | { Cohttp.Response.status = `OK; _ } -> body |> Cohttp_lwt_body.to_string
                        | { Cohttp.Response.status; _ } -> body |> Cohttp_lwt_body.to_string) in
    ()

 
 let dropTable tableName = 
    let stringURI = concat "" ["http://localhost:3000/dropTable"; "/"; tableName] in
    let headers = Header.add (Header.init ()) "content-type" "application/json" in
    let bodyString = "{}" in
    let post = Lwt_main.run (Client.post ~headers ~body:(Cohttp_lwt_body.of_string bodyString) (Uri.of_string stringURI) >>= fun (response, body) ->
                        match response with
                        | { Cohttp.Response.status = `OK; _ } -> body |> Cohttp_lwt_body.to_string
                        | { Cohttp.Response.status; _ } -> body |> Cohttp_lwt_body.to_string) in
    ()

let rec range i j k = if i > j then [] else i::(range (i+k) j k)

let rev list =
    let rec aux acc = function
      | [] -> acc
      | h::t -> aux (h::acc) t in
    aux [] list;;

let gen_passwd length =
    let gen() = match Random.int(26+26+10) with
        n when n < 26 -> int_of_char 'a' + n
      | n when n < 26 + 26 -> int_of_char 'A' + n - 26
      | n -> int_of_char '0' + n - 26 - 26 in
    let gen _ = String.make 1 (char_of_int(gen())) in
    String.concat "" (Array.to_list (Array.init length gen));;

let genUuid x = 
    let part1 = List.fold_left (fun a x -> a ^ (string_of_int (Random.int 10))) "" (range 1 8 1) in
    let part2 = List.fold_left (fun a x -> a ^ (string_of_int (Random.int 10))) "" (range 1 4 1) in
    let part3 = List.fold_left (fun a x -> a ^ (string_of_int (Random.int 10))) "" (range 1 4 1) in 
    let part4 = List.fold_left (fun a x -> a ^ (string_of_int (Random.int 10))) "" (range 1 4 1) in 
    let part5 = List.fold_left (fun a x -> a ^ (string_of_int (Random.int 10))) "" (range 1 12 1) in
    let uuid = concat "" [part1; "-"; part2; "-"; part3; "-"; part4;"-";part5] in
    uuid

let runSession num_ops =
    let sessionId = genUuid 10 in
    let seqNos = range 1 num_ops 2 in
    List.map (fun seqNo ->
		let user_name = gen_passwd 10 in
                let password = gen_passwd 10 in  
		do_add_user user_name password sessionId 1;
		(*Unix.sleep 2;*)
		get_user_id_by_name user_name) (rev seqNos)

let time f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx

let () = 
    let num_ops = 10 in
    let num_threads = 128 in
    let tableNames = ["username"; "user"; "tweet"; "timeline"; "userline"] in
    let x = List.map (fun tableName -> dropTable tableName) tableNames in 
    let x = List.map (fun tableName -> createTable tableName) tableNames in 
    let x = List.map (fun tableName -> notifyAboutTable tableName 3000;notifyAboutTable tableName 3001;notifyAboutTable tableName 3002) tableNames in
    let threads_list = range 1 num_threads 1 in
    time Lwt.join (List.map (fun x -> Printf.printf "Thread %d is executing\n" x;runSession num_ops; Lwt.return ()) threads_list);
    ()
