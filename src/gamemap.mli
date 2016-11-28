(* map.mli *)
open Player
(* A [Map] represents a location that contains info about action items
* on that location. *)
module type Gamemap = sig
  (* The type of a location *)
  type location

  (* the type of actions - stop event, just points...etc *)
  type action

  type direction

  type player

  type square

  (* [moveforward location] returns another location that signals the
   * player where to go next *)
  val moveforward: location -> direction -> (square * action) list -> player -> location
end

