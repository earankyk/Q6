open Q6_interface
open Sexplib
open Sexplib.Std

module List = 
struct
  open List
  let rec map f l = match l with
    | [] -> []
    | x::xs -> (f x)::(map f xs)

  let rec concat ls = 
    let rec append l1 l2 = match l1 with
      | [] -> l2
      | x::xs -> x::(append xs l2) in
      match ls with
        | [] -> []
        | l::rest -> append l (concat rest)

  let rec fold_left f b l = match l with
    | [] -> b
    | x::xs -> fold_left f (f b x) xs

  let rec fold_right f l b = match l with
    | [] -> b
    | x::xs -> f x (fold_right f xs b)

  let rec iter f l = match l with
    | [] -> ()
    | x::xs -> (f x; iter f xs)

  let rec first_some l = match l with
    | [] -> None
    | x::xs -> (match x with 
                  | None -> first_some xs
                  | Some _ -> x)

  let rec forall l f = match l with
    | [] -> true
    | x::xs -> (f x)&&(forall xs f)

  let rec exists l f = match l with
    | [] -> false
    | x::xs -> (f x)||(exists xs f)
end

module L =
struct
  let forall l f = true
  let exists l f = true
end

module User = 
struct
  type id = int (*Uuid.t*) [@@deriving sexp]
  type usernamePwd = {username: string; pwd: string} [@@deriving sexp]
  type leaderId = {leader_id: id} [@@deriving sexp]
  type followerId = {follower_id: id} [@@deriving sexp]
  (*type eff = Add of {username: string; pwd: string}
    | AddFollowing of {leader_id: id}
    | RemFollowing of {leader_id: id}
    | AddFollower of {follower_id: id}
    | RemFollower of {follower_id: id} 
    | Blocks of {follower_id: id}
    | GetBlocks
    | IsBlockedBy of {leader_id: id}
    | GetIsBlockedBy
    | GetInfo
    | GetFollowers
    | GetFollowing [@@deriving sexp]*)
  type eff = Add of usernamePwd
    | AddFollowing of leaderId
    | RemFollowing of leaderId
    | AddFollower of followerId
    | RemFollower of followerId
    | Blocks of followerId
    | GetBlocks
    | IsBlockedBy of leaderId
    | GetIsBlockedBy
    | GetInfo
    | GetFollowers
    | GetFollowing [@@deriving sexp]
  let effToString x = (*Sexp.to_string_hum*) Sexp.string_of_sexp (sexp_of_eff x) 
  let stringToEff x = eff_of_sexp (sexp_of_string x)
  let id_to_str = (*Uuid.to_string*) string_of_int
end

module User_table =
struct
  include Store_interface.Make(User)
end

module UserName = 
struct
  type id = string [@@deriving sexp]
  type userId = {user_id: User.id} [@@deriving sexp]
  type eff = Add of userId | GetId [@@deriving sexp]
  let effToString x = Sexp.to_string_hum (sexp_of_eff x)
  let stringToEff x = eff_of_sexp (sexp_of_string x)
  let id_to_str x = x
end
module UserName_table =
struct
  include Store_interface.Make(UserName)
end

module Tweet =
struct
  type id = int (*Uuid.t*) [@@deriving sexp]
  type authorContent = {author_id:User.id; content:string} [@@deriving sexp]
  type eff = New of authorContent 
    | Get [@@deriving sexp]
  let effToString x = Sexp.to_string_hum (sexp_of_eff x)
  let stringToEff x = eff_of_sexp (sexp_of_string x)
  let id_to_str = string_of_int 
end
module Tweet_table =
struct
  include Store_interface.Make(Tweet)
end

module Timeline = 
struct
  type id = User.id [@@deriving sexp]
  type tweetId = {tweet_id:Tweet.id} [@@deriving sexp]
  type eff = NewTweet of tweetId
    | Get [@@deriving sexp]
  let effToString x = Sexp.to_string_hum (sexp_of_eff x)
  let stringToEff x = eff_of_sexp (sexp_of_string x)
  let id_to_str = (*Uuid.to_string*) string_of_int
end
module Timeline_table =
struct
  include Store_interface.Make(Timeline)
end

module Userline = 
struct
  type id = User.id [@@deriving sexp]
  type tweetId = {tweet_id:Tweet.id} [@@deriving sexp]
  type eff = NewTweet of tweetId
    | Get [@@deriving sexp]
  let effToString x = Sexp.to_string_hum (sexp_of_eff x)
  let stringToEff x = eff_of_sexp (sexp_of_string x)
  let id_to_str = (*Uuid.to_string*) string_of_int
end
module Userline_table =

struct
  include Store_interface.Make(Userline)
end


(*
 * There is a zip/map2 bug unfixed. Fix goes in mkfun 
 * inside rdtextract.ml
 *) 
let do_test1 uid name sessionId seqNo = 
  let x = [1;2] in
  let y = UserName.GetId in
  let z = UserName.Add {user_id=uid} in
  let u1 = UserName_table.append name z "username" sessionId seqNo in
  let u2 = UserName_table.get name "username" y in
  let u3 = List.map 
             (fun eff -> match eff with
                | Some (UserName.Add {user_id=uid'}) -> Some uid'
                | _ -> None) u2 in
  let u4 = List.fold_right (fun idop acc -> match idop with
                             | Some uid' -> uid'::acc
                             | None -> acc) u3 [] in
    u4

let do_add_user name pwd sessionId seqNo = 
  let uid = Random.int 1000000 (*Uuid.create()*) in
  begin
    UserName_table.append name (UserName.Add {user_id=uid}) "username" sessionId seqNo;
    User_table.append uid (User.Add {username=name;pwd=pwd}) "username" sessionId seqNo
  end 

let get_user_id_by_name nm = 
  let ctxt = (* ea *) UserName_table.get "username" nm (UserName.GetId) in
  let ids = List.map (fun eff -> match eff with 
                        | Some (UserName.Add {user_id=id}) -> Some id 
                        | _ -> None) ctxt in
  let num_ids = List.fold_right (fun idop acc -> match idop with
                                   | None -> acc
                                   | Some _ -> 1 + acc) ids 0 in
    begin
      if num_ids > 1 then raise Inconsistency else ();
      List.first_some ids
    end

let do_block_user me other sessionId seqNo = 
  let Some my_id = get_user_id_by_name me in
  let Some other_id = get_user_id_by_name other in
    begin
      User_table.append my_id (User.Blocks {follower_id=other_id}) "User" sessionId seqNo;
      User_table.append other_id (User.IsBlockedBy {leader_id=my_id}) "User" sessionId seqNo;
      User_table.append my_id (User.RemFollower {follower_id=other_id}) "User" sessionId seqNo; 
     User_table.append other_id (User.RemFollowing {leader_id=my_id}) "User" sessionId seqNo
    end

let do_new_tweet uid str sessionId seqNo = 
  let ctxt = User_table.get uid "user" (User.GetFollowers) in
  let fids = List.map 
               (fun eff -> match eff with 
                  | Some x -> 
                      (match x with 
                         | User.AddFollower {follower_id=fid} -> Some fid
                         | _ -> None)
                  | _ -> None) ctxt in
  let tweet_id = Random.int 1000000 (*Uuid.create()*) in
    begin
      Tweet_table.append tweet_id (Tweet.New {author_id=uid; content=str}) "tweet" sessionId seqNo;
      Userline_table.append uid (Userline.NewTweet {tweet_id=tweet_id}) "tweet" sessionId seqNo;
      List.iter 
        (fun fidop -> match fidop with 
           | Some fid -> Timeline_table.append fid 
                      (Timeline.NewTweet {tweet_id=tweet_id}) "timeline" sessionId seqNo
           | None -> ()) fids;
    end

let get_tweet tid = 
  let ctxt = Tweet_table.get tid "tweet" (Tweet.Get) in
  let tweets = List.map (fun eff -> match eff with
                           | Some (Tweet.New {content}) -> Some content
                           | _ -> None) ctxt in
  let res = List.first_some tweets in
    res

let inv_fkey uid' = 
  List.forall (Userline_table.get uid' "userline" (Userline.Get))
    (fun e -> match e with
       | Some x -> 
           (match x with 
              | Userline.NewTweet {tweet_id=tid} -> 
                  List.exists (Tweet_table.get tid "userline" Tweet.Get) 
                    (fun e' -> match e' with 
                       | Some y -> (match y with 
                                      | (Tweet.New _) -> true 
                                      | _ -> false)
                       | _ -> false)
              | _ -> false)
       | _ -> true)

let get_userline uid = 
  let ctxt = Userline_table.get uid "userline" (Userline.Get) in
  let tweets = List.map 
                 (fun x -> match x with 
                    | Some y -> 
                        (match y with 
                           | Userline.NewTweet {tweet_id=tid} -> 
                                Some (get_tweet tid)
                           | _ -> None)
                    | _ -> None) ctxt in
  (*let _ = List.iter (fun top -> match top with 
                           | Some x ->  (match x with
                                           | Some _ -> ()
                                           | None -> raise Inconsistency)
                           | None -> ()) 
            tweets in*)
    tweets
