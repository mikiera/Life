(* map.ml *)

module Map : sig
  type location

  type action

  val moveforward: location -> location
end =
struct
  type location = unit

  type action = unit

  let moveforward (current_loc : location) : location =
    failwith "Unimplemented"
end

