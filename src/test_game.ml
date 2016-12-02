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
let loc2 = {id = Square 2; left = Null; right = Square 3}
let loc3 = {id = Square 3; left = Null; right = Square 4}
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

let ploc_lst = [(1, {loc = loc1; dir = Right}); (2, {loc = loc2; dir = Left})]
let ploc_lst2 = [(1, {loc = loc2; dir = Right}); (2, {loc = loc2; dir = Left})]
let ploc_lst3 = [(1, {loc = loc1; dir = Right}); (2, {loc = loc3; dir = Left})]
let ploc_lst4 = [(1, {loc = loc3; dir = Right}); (2, {loc = loc2; dir = Left})]
let sqact_lst = [(Square 1, action1); (Square 2, action2)]
let card_lst1 = [{name = "1110"; description = ""; id = 1; points = 10; karma = 2; card_type = ChoiceC}; {name = "2110"; description = ""; id = 2; points = 15; karma = 3; card_type = ChoiceC}]
let card_lst2 = [{name = "White"; description = ""; id = 1; points = 2; karma = 3; card_type = ChoiceA}; {name = "Gries"; description = ""; id = 2; points = 3; karma = 4; card_type = ChoiceA}]
let card_lst3 = [{name = "Job"; description = ""; id = 1; points = 34; karma = 4; card_type = ChoiceS}; {name = "Internship"; description = ""; id = 2; points = 100; karma = 200; card_type = ChoiceS}]
let card_lst4 = [{name = "PhD"; description = ""; id = 1; points = 12; karma = 2; card_type = ChoiceF}; {name = "Masters"; description = ""; id = 2; points = 24; karma = 34; card_type = ChoiceF}]
let gcomp = {courses = card_lst1; advisors = card_lst2; summer = card_lst3; future = card_lst4}
let pcard_lst = [(1, [List.hd card_lst1; List.hd card_lst2]); (2, [List.nth card_lst1 1; List.nth card_lst2 1])]
let gs = {turn = 1; playermap = ploc_lst; sqact = sqact_lst; start = 1; start_points = 2; gamemap = locations; gamecomp = gcomp; players = player_lst; playercard = pcard_lst; active_players = [1;2]}
let gs1 = {turn = 1; playermap = ploc_lst2; sqact = sqact_lst; start = 1; start_points = 2; gamemap = locations; gamecomp = gcomp; players = player_lst; playercard = pcard_lst; active_players = [1;2]}
let gs2 = {turn = 1; playermap = ploc_lst3; sqact = sqact_lst; start = 1; start_points = 2; gamemap = locations; gamecomp = gcomp; players = player_lst; playercard = pcard_lst; active_players = [1;2]}
let gs3 = {turn = 1; playermap = ploc_lst4; sqact = sqact_lst; start = 1; start_points = 2; gamemap = locations; gamecomp = gcomp; players = player_lst; playercard = pcard_lst; active_players = [1;2]}
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

  "test_find_loc_by_sid4" >:: (fun _ -> assert_raises (Failure "This square id is not in the game.")
                              (fun () -> (find_loc_by_sid locs (Square 3))));

  "test_find_loc_by_sid5" >:: (fun _ -> assert_raises (Failure "This is not a valid square id.")
                              (fun () -> (find_loc_by_sid locs (Null))));

  "move_one_step1" >:: (fun _ -> assert_equal gs1 (move_one_step gs 1));

  "move_one_step2" >:: (fun _ -> assert_equal gs2 (move_one_step gs 2));

  "move_one_step3" >:: (fun _ -> assert_equal gs3 (move_one_step gs1 1));


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