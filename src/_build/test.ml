open OUnit2
open Player

type history = string ref
type player = {id:int; nickname: string ref; history: history;
                  college: string ref; course: string ref;
                  advisor: string ref; points: int ref; karma: int ref;
                  retirement: string ref}
let p1 = {id = 1; nickname = ref "HB"; history = ref ""; college = ref "ENG";
          course = ref "CS"; advisor = ref "hello"; points = ref 0;
          karma = ref 10; retirement = ref "student"}

let tests = "Life test suite" >:::[
  "getID" >:: (fun _ -> assert_equal 1 (getID p1));
]

let _ = run_test_tt_main tests