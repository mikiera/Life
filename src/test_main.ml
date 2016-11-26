(* test_main.ml
 * Contains test cases used for Life of 3110 Project
 *)
open OUnit2

let test_suite = "Life test suite" >:::
  Test_game.tests @ Test_player.tests @ Test_map.tests

let _ = run_test_tt_main test_suite