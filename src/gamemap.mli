(* map.mli *)
open Player
(* A [Map] represents a location that contains info about action items
* on that location. *)
module Gamemap : sig
  (* The type of a location *)
  type square = Null | Square of int

  type location = {id: square; left: square; right: square}

  type actionType = Event | ChoiceC | ChoiceA | ChoiceS | ChoiceF

  type action = {
      actionType: actionType;
      description: string;
      points: int;
      karma: int
  }

  type direction = Left | Right

  type player

  (* [moveforward location] returns another location that signals the
   * player where to go next *)
  val moveforward: location -> direction -> (square * action) list -> player -> location
end

