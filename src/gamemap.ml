open Player

module type Gamemap = sig
  type location

  type action

  type direction

  type player

  type square

  val moveforward: location -> direction -> (square * action) list -> player -> location
end

module OurMap : Gamemap = struct
  type square = None | Square of int
  type location = {id: square; left: location; right: location}
  type direction = Left | Right
  type actionType = Event | Choice
  type action = {
  		actionType: actionType;
  		description: string;
  		points: int;
  		karma: int
  }
  type player = Player.player

  let moveforward current_loc dir map_action_list player =
    if ((dir = Right && current_loc.right.id != None) || current_loc.left.id = None)
    	then let action_item = List.assoc current_loc.right.id map_action_list in
    		  (Player.changePoints) player action_item.points;
    		  (Player.changeKarma) player action_item.karma;
    		  current_loc.right
	else if (dir = Left && current_loc.left.id != None)
		then let action_item = List.assoc current_loc.left.id map_action_list in
    		  (Player.changePoints) player action_item.points;
    		  (Player.changeKarma) player action_item.karma;
		current_loc.left
	else current_loc
end