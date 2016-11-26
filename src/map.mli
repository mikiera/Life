
(* A [Map] represents a location that contains info about action items
 * on that location. *)
module type Map = sig
	(* The type of a location *)
	type location

  (* the type of actions - stop event, just points...etc *)
  type action

	(* [moveforward location] returns another location that signals the
	 * player where to go next *)
	val moveforward : location -> location
end

