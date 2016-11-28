(* map.ml *)
open Player

module Gamemap : sig
  type location

  type action
  type player

  val moveforward: location -> Player.player -> location
end =
struct
  type location = unit

  type action = unit
  type player = Player.player

  let moveforward (current_loc : location) (p:Player.player) : location =
    failwith "Unimplemented"
end

let main file = failwith "Unimplemented"