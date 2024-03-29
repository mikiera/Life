(* test_player.ml
 * Contains test cases used for module Player
 *)
open OUnit2
open Player

let p1 = Player.changeFuture (Player.changeKarma (Player.changeKarma
    (Player.changePoints (Player.changePoints (Player.changeAdvisor
    (Player.changeCourse (Player.changeCollege (Player.addNickname
    (Player.changeSummerPlans (Player.createPlayer 1) "Internship")
    "Tom") "Engineering") "CS 3110") "Nick") 10) 10) 5) 10) "MEng"
let p2 = Player.addNickname (Player.createPlayer 2) "Jerry"

let tests = [
    "getID1" >:: (fun _ -> assert_equal 1 (Player.getID p1));
    "getID2" >:: (fun _ -> assert_equal 2 (Player.getID p2));
    "getName1" >:: (fun _ -> assert_equal ("Tom") (Player.getNickname p1));
    "getName2" >:: (fun _ -> assert_equal ("Jerry") (Player.getNickname p2));
    "getCollege" >:: (fun _ -> assert_equal ("Engineering") (Player.getCollege p1));
    "getCourse" >:: (fun _ -> assert_equal ("CS 3110") (Player.getCourse p1));
    "getAdvisor" >:: (fun _ -> assert_equal ("Nick") (Player.getAdvisor p1));
    "getPoints" >:: (fun _ -> assert_equal 20 (Player.getPoints p1));
    "getKarma" >:: (fun _ -> assert_equal 15 (Player.getKarma p1));
    "getRetirement" >:: (fun _ -> assert_equal "MEng" (Player.getFuture p1));
    "getHistory" >:: (fun _-> assert_equal ("ID: 1\nChanged Summer Plans to: Internship\nName: Tom\nCollege: Engineering\nChanged Course to: CS 3110\nChanged Advisor to: Nick\n") (Player.getHistory p1));
    "getSummerPlans" >:: (fun _ -> assert_equal "Internship" (Player.getSummerPlans p1));

    "changeCollege" >:: (fun _ -> assert_equal "AAP"
        (Player.getCollege (Player.changeCollege p1 "AAP")));
    "changeCourse" >:: (fun _ -> assert_equal "Graphics"
        (Player.getCourse (Player.changeCourse p1 "Graphics")));
    "changeAdvisor" >:: (fun _ -> assert_equal "Clarkson"
        (Player.getAdvisor (Player.changeAdvisor p1 "Clarkson")));
    "changePoints" >:: (fun _ -> assert_equal 54
        (Player.getPoints (Player.changePoints p1 34)));
    "changeKarma" >:: (fun _ -> assert_equal 76
        (Player.getKarma (Player.changeKarma p1 61)));
    "changeFuture" >:: (fun _ -> assert_equal "Emptiness"
        (Player.getFuture (Player.changeFuture p1 "Emptiness")));
    "changeSummerPlans" >:: (fun _ -> assert_equal "Sleep"
        (Player.getSummerPlans (Player.changeSummerPlans p1 "Sleep")));
]