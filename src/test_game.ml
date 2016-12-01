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
let college = init_s.gamecomp.college
let advisors = init_s.gamecomp.advisors
let summer = init_s.gamecomp.summer

let p1 = Player.createPlayer 1
let p2 = Player.createPlayer 2
let player_lst = p1::[p2]

let loc1 = {id = Square 1; left = Null; right = Square 2}
let loc2 = {id = Square 2; left = Square 1; right = Square 3}
let locations = loc1::[loc2]

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

  "test_find_loc_by_sid1" >:: (fun _ -> assert_equal (loc1) (find_loc_by_sid locations (Square 1)));

  "test_find_loc_by_sid2" >:: (fun _ -> assert_equal (loc2) (find_loc_by_sid locations (Square 2)));

]