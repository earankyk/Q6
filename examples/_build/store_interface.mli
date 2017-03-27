open SessionIdMap

module type TABLE_TYPE = 
sig
  type id
  type eff
  val id_to_str : id -> string
  val stringToEff : string -> eff
  val effToString : eff -> string
end
module Make : functor(Table : TABLE_TYPE) ->
sig
  val get : Table.id -> string -> Table.eff -> (*Table.eff list (*option*)*)Table.eff option list
  val append : Table.id -> Table.eff -> string -> string -> int -> unit
end
