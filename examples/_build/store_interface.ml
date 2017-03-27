open Lwt
open Cohttp
open Cohttp_lwt_unix
open String
open Yojson
open SessionIdMap
open Hashtbl

module type TABLE_TYPE = 
sig
  type id
  type eff
  val id_to_str : id -> string
  val stringToEff : string -> eff
  val effToString : eff -> string
end

module Make = functor(S : TABLE_TYPE) ->
struct

  let m = SessionIdMap.empty;;

  let get id objType read_eff = 
    let stringId = S.id_to_str id in 
    let stringURI = concat "" ["http://localhost:3000/read/"; stringId;"/";objType] in
    let requestBody = Client.get (Uri.of_string stringURI) >>= fun (resp, body) ->
    body |> Cohttp_lwt_body.to_string >|= fun body -> Yojson.Basic.from_string body in
    let json = Lwt_main.run requestBody in
    let effectsList = json |> Yojson.Basic.Util.to_list in
    match effectsList with
    | [] -> [None]
    | effects -> 
	List.map 
	    (fun json -> let effect = Yojson.Basic.Util.member 
				      "effect" json |> 
				      Yojson.Basic.to_string |>
                                      fun x -> let len = String.length x in
				      	       sub x 1 (len - 2) |> 
				      fun x -> Printf.printf "Reading : %s\n" x; 
				      S.stringToEff x in
                                      Some effect) effects

  let append id write_eff objType sessionId seqNo =
    (*let m = SessionIdMap.touch m sessionId in
    let seqNo = SessionIdMap.find m sessionId in*)
    let addr = 3000 + ((hash sessionId) mod 3) in
    let _ = Printf.printf "%s\n" sessionId in
    let stringURI = ("http://localhost:"^(string_of_int addr))^("/append") in
    let headers = Header.add (Header.init ()) "content-type" "application/json" in
    let bodyString = String.concat "" ["{\"objType\":"; "\""; objType; "\", \"sessionId\":\""; sessionId;"\",\"key\":\""; (S.id_to_str id); "\", "; "\"effectString\":";"\""; (S.effToString write_eff); "\",\"seqNo\":";(string_of_int seqNo);"}"] in
    Printf.printf "Appending : %s\n" (S.effToString write_eff);
    let post = Lwt_main.run (Client.post ~headers ~body:(Cohttp_lwt_body.of_string bodyString) (Uri.of_string stringURI) >>= fun (response, body) ->
                        match response with
                        | { Cohttp.Response.status = `OK; _ } -> body |> Cohttp_lwt_body.to_string
                        | { Cohttp.Response.status; _ } -> body |> Cohttp_lwt_body.to_string) in
    (*Printf.printf "%s\n" post;*)
    ()

end
