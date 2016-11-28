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
    "getName1" >:: (fun _ -> assert_equal ("Tom") (Player.getNickname (Player.addNickname p1 "Tom")));
    "getName2" >:: (fun _ -> assert_equal ("Jerry") (Player.getNickname (Player.addNickname p2 "Jerry")));
    "getCollege" >:: (fun _ -> assert_equal ("Engineering") (Player.getCollege (Player.changeCollege p1 "Engineering")));
    "getCourse" >:: (fun _ -> assert_equal ("CS 3110") (Player.getCourse (Player.changeCourse p1 "CS 3110")));
    "getAdvisor" >:: (fun _ -> assert_equal ("Nick") (Player.getAdvisor (Player.changeAdvisor p1 ("Nick"))));
    "getPoints1" >:: (fun _ -> assert_equal 10 (Player.getPoints (Player.changePoints p1 10)));
    "getPoints2" >:: (fun _ -> assert_equal 20 (Player.getPoints (Player.changePoints p1 10)));
    "getKarma1" >:: (fun _ -> assert_equal 5 (Player.getKarma (Player.changeKarma p1 5)));
    "getKarma2" >:: (fun _ -> assert_equal 15 (Player.getKarma (Player.changeKarma (p1) 10)));
    "getHistory" >:: (fun _-> assert_equal ("ID: 1\nName: Tom\nCollege: Engineering\nCourse: CS 3110\nAdvisor: Nick\n") (Player.getHistory p1));
]