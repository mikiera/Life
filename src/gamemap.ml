open Player

module Gamemap : sig
  type location

  type action

  type direction

  type player

  type square

  val moveforward: location -> direction -> (square * action) list -> player -> location
end
= struct
  type square = Null | Square of int
  type location = {id: square; left: square; right: square}
  type direction = Left | Right
  type actionType = Event | ChoiceC | ChoiceA | ChoiceS | ChoiceF
  type action = {
  		actionType: actionType;
  		description: string;
  		points: int;
  		karma: int
  }
  type player = Player.player

  let moveforward current_loc dir map_action_list player =
(*     if ((dir = Right && current_loc.right.id <> Null) || current_loc.left.id = Null)
    	then let action_item = List.assoc current_loc.right.id map_action_list in
    		  ignore((Player.changePoints) player action_item.points);
    		  ignore((Player.changeKarma) player action_item.karma);
    		  current_loc.right
	else if (dir = Left && current_loc.left.id <> Null)
		then let action_item = List.assoc current_loc.left.id map_action_list in
    		  ignore((Player.changePoints) player action_item.points);
    		  ignore((Player.changeKarma) player action_item.karma);
		current_loc.left
	else current_loc *)
    failwith "Unimplemented"
end