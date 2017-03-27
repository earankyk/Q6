open Core.Std

module SessionIdMap = 
struct
  type t = int String.Map.t
  let empty = String.Map.empty
  let find sessionId s = 
      match Map.find sessionId s with
      | None -> 0
      | Some x -> x
  let touch t s =
    let count =
      match Map.find t s with
      | None -> 0
      | Some x -> x
    in
    Map.add t s (count + 1)
end
