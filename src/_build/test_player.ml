(* test_player.ml
 * Contains test cases used for module Player
 *)
open OUnit2
open Player

let p1 = Player.createPlayer 1
let p2 = Player.createPlayer 2

let tests = [
    "getID1" >:: (fun _ -> assert_equal 1 (Player.getID p1));
    "getID2" >:: (fun _ -> assert_equal 2 (Player.getID p2));
]