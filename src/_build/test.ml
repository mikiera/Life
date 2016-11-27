open OUnit2
open Player

let p1 = Player.createPlayer 1
let p2 = Player.createPlayer 2

let tests = "Life test suite" >:::[
  "getID" >:: (fun _ -> assert_equal 1 (Player.getID p1));
]