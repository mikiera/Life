(* test_game.ml
 * Contains test cases used for module Game
 *)
open OUnit2
open Game

let s = Yojson.Basic.from_file "a.json"

let init_s = s |> init_game
let gamemap = init_s.gamemap
let sqact = init_s.sqact
let advisors = init_s.gamecomp.advisors
let summer = init_s.gamecomp.summer

let tests = [

  (* check parsing *)

  "start" >::
    (fun _ -> assert_equal 1 (let state = (init_game s) in state.start));

  "start points" >:: (fun _ -> assert_equal 10000
    (let state = (init_game s) in state.start_points));











]