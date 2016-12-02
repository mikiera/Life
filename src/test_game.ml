(* test_game.ml
 * Contains test cases used for module Game
 *)
open OUnit2
open Game
open Player

let s = Yojson.Basic.from_file "a.json"

let init_s = s |> init_game
let gamemap = init_s.gamemap
let sqact = init_s.sqact
let advisors = init_s.gamecomp.advisors
let summer = init_s.gamecomp.summer

let p1 = Player.createPlayer 1
let p2 = Player.createPlayer 2
let player_lst = p1::[p2]

let loc1 = {id = Square 1; left = Null; right = Square 2}
let loc2 = {id = Square 2; left = Square 1; right = Square 3}
let locs = loc1::[loc2]

let locations = [{id = Square 1; left = Null; right = Square 2};
				 {id = Square 2; left = Null; right = Square 3};
				 {id = Square 3; left = Null; right = Square 4};
				 {id = Square 4; left = Null; right = Square 5};
				 {id = Square 5; left = Null; right = Null}]

let action1 = {actionType = Event;
			   description = "event. should not be printed";
  			   points = 0;
  			   karma = 0}

let action2 = {actionType = ChoiceCol;
   			   description = "You get to choose between Arts and Sciences or Engineering.";
   			   points = 0;
   			   karma = 0}

let action3 = {actionType = Event;
			   description = "You got a 100 on your first CS assignment. Congratulations! You get 500 points.";
			   points = 500;
			   karma = 0}

let action4 = {actionType = ChoiceA;
	  		   description = "You need to choose an advisor. You also get karma points.";
	  		   points = 600;
	  		   karma = 1000}

let action5 = {actionType = ChoiceF;
	  		   description = "Congratulations! You have graduated!! Now you get to choose your post-Cornell plans. You also get karma points.";
	  		   points = 0;
	  		   karma = 1000}

let square_action_lst = [(Square 1, action1);(Square 2, action2); (Square 3, action3); (Square 4, action4); (Square 5, action5)]

(* let course1 = {name = "Graphics";} *)

let tests = [

  (* check parsing *)

  "start" >::
    (fun _ -> assert_equal 1 (let state = (init_game s) in state.start));

  "start points" >:: (fun _ -> assert_equal 10000
    (let state = (init_game s) in state.start_points));

  "test_cmd1" >:: (fun _ -> assert_equal ("quit") (cmd_checker "QuIt"));

  "test_cmd2" >:: (fun _ -> assert_equal ("spin") (cmd_checker "SpIn"));

  "test_find_player_by_id1" >:: (fun _ -> assert_equal (p1) (find_player_by_id player_lst 1));

  "test_find_player_by_id2" >:: (fun _ -> assert_equal (p2) (find_player_by_id player_lst 2));

  "test_find_player_by_id3" >:: (fun _ -> assert_raises (Failure "This player id is not in the game.")
                                (fun () -> (find_player_by_id player_lst 3)));

  "test_find_loc_by_sid1" >:: (fun _ -> assert_equal (loc1) (find_loc_by_sid locs (Square 1)));

  "test_find_loc_by_sid2" >:: (fun _ -> assert_equal (loc2) (find_loc_by_sid locs (Square 2)));

  "test_find_loc_by_sid4" >:: (fun _ -> assert_raises (Failure "This square id is not in the game."
                              (fun () -> (find_loc_by_sid locs))))

  "start turn" >:: (fun _ -> assert_equal 0
    (let state = (init_game s) in state.turn));

  "start gamemap" >:: (fun _ -> assert_equal locations
    (let state = (init_game s) in state.gamemap));

  "start sqact" >:: (fun _ -> assert_equal square_action_lst
    (let state = (init_game s) in state.sqact));

  "start players" >:: (fun _ -> assert_equal []
    (let state = (init_game s) in state.players));

  "start active_players" >:: (fun _ -> assert_equal []
    (let state = (init_game s) in state.active_players));

]