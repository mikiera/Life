open OUnit2
open Player

let tests = "Life test suite" >:::[
  "getID" >:: (fun _ -> assert_equal 1 (Player.getID p1));
]

let _ = run_test_tt_main tests