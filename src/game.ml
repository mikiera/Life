(* game.ml *)
open Gamemap
open Player
open Random

exception Illegal

type turn = int

type square = Null | Square of int

type location = {id: square; left: square; right: square}

type actionType = Event | ChoiceC | ChoiceA | ChoiceS | ChoiceF

type action = {
      actionType: actionType;
      description: string;
      points: int;
      karma: int
  }

type playerloc = {playerid: int; loc: location}

type college = AS | Eng | NONE

type cardtype = COURSE | ADVISOR | SUMMER | FUTURE

type card = {name: string;
             description: string;
             id: int;
             points: int;
             karma: int;
             card_type: cardtype}

type gamecomp = {college: college;
                 courses: card list;
                 advisors: card list;
                 summer: card list;
                 future: card list}

type gamestate = {turn: turn;
                  playermap: playerloc list;
                  sqact: (square * action) list;
                  start: int;
                  start_points: int;
                  gamemap: location list;
                  gamecomp: gamecomp; active_players: int list}

let cmd_checker c =
  let a = String.lowercase_ascii (String.trim c) in a

let play (cmd : string) (gamestate : gamestate) : gamestate =
(*   if (cmd = "p" || cmd = "points") then (print_endline (Player.getPoints (List.nth (player_lst) turn)); gamestate)
  else if (cmd = "h" || cmd = "history") then (print_endline (Player.getHistory (List.nth (player_lst) turn)); gamestate)
  else if (cmd = "a" || cmd = "advisor") then (print_endline (Player.getAdvisor (List.nth (player_lst) turn)); gamestate)
  else if (cmd = "c" || cmd = "courses") then (print_endline (Player.getCourse (List.nth (player_lst) turn)); gamestate)
  else if (cmd = "co" || cmd = "college") then (print_endline (Player.getCollege (List.nth (player_lst) turn)); gamestate)
  else if (cmd = "n" || cmd = "name") then (print_endline (Player.getNickname (List.nth (player_lst) turn)); gamestate)
  else if (cmd = "spin") then ((Random.int 4) + 1)
  else if (cmd = "help") then (print_endline ("p/points:      check your total points");
                              print_endline ("a/advisor:     see your advisor");
                              print_endline ("c/courses:     see your courses");
                              print_endline ("co/college:    see your college");
                              print_endline ("n/name:        see your nickname");
                              print_endline ("spin:          spin the wheel and try your luck!");
                              print_endline ("help:          see a list of commands available");
                              gamestate)
  else if (cmd = "Choice 1") then failwith "Unimplemented"
  else if (cmd = "Choice 2") then failwith "Unimplemented"
  else raise Illegal *)
  failwith "Unimplemented"

let rec repl (state : gamestate)  =
  print_string "> ";
  let c = read_line () in
  let a = cmd_checker c in
  if (a = "quit" || a = "exit" || a = "q") then ()
  else try(let new_gs = play (cmd_checker c) state in repl new_gs) with
  |Illegal -> print_endline "Invalid command. Please try again."; repl state


(* parsing functions *)

let extract_card ctype card =
  let open Yojson.Basic.Util in
  let name = card |> member "name" |> to_string in
  let desc = card |> member "description" |> to_string in
  let id = card |> member "id" |> to_int in
  let points = card |> member "points" |> to_int in
  let karma = card |> member "karma" |> to_int in
  {name=name; description = desc; id = id; points= points;
  karma=karma; card_type = ctype}


let make_loc_list loc : location =
  let open Yojson.Basic.Util in
  let id = loc |> member "squareid" |> to_int in
  let realid = if id <> 0 then Square id else Null in
  let left = loc |> member "left" |> to_int in
  let realleft = if left <> 0 then Square left else Null in
  let right = loc |> member "right" |> to_int in
  let realright = if right <> 0 then Square right else Null in
  {id = realid; left = realleft; right = realright}


let parse_action action : action =
  let open Yojson.Basic.Util in
  let atype = action |> member "type" |> to_string in
  let finaltype = begin match atype with
    | "event" -> Event
    | "course" -> ChoiceC
    | "advisor" -> ChoiceA
    | "summer" -> ChoiceS
    | "future" -> ChoiceF
    | _ -> Event  end
  in
  let description = action |> member "description" |> to_string in
  let points = action |> member "points" |> to_int in
  let karma = action |> member "karma" |> to_int in
  {actionType = finaltype; description = description; points = points;
  karma = karma}


let sq_act_list sqact =
  let open Yojson.Basic.Util in
  let id = sqact |> member "squareid" |> to_int in
  let finalid = if id <> 0 then Square id else Null in
  let action = sqact |> member "action" |> parse_action in
  (finalid, action)

let init_game j =
  let open Yojson.Basic.Util in
  let courses = j |> member "game_components" |> member "courses" |> to_list
    |> List.map (extract_card COURSE) in
  let advisors = j |> member "game_components" |> member "advisors" |> to_list
    |>  List.map (extract_card ADVISOR) in
  let summer = j |> member "game_components" |> member "summer_plans" |> to_list
    |>  List.map (extract_card SUMMER) in
  let future = j |> member "game_components" |> member "future_plans" |> to_list
    |>  List.map (extract_card FUTURE) in
  let gamecomp = {college = NONE; courses = courses; advisors = advisors;
  summer = summer; future = future } in
  let start = j |> member "start" |> to_int in
  let start_points = j |> member "start_points" |> to_int in
  let gamemap = j |> member "map" |> to_list |> List.map make_loc_list in
  let sqact = j |> member "square_action" |> to_list |> List.map sq_act_list in
  {turn=0; playermap = [];
  gamecomp = gamecomp;
  active_players = [];
  start = start;
  start_points = start_points;
  gamemap = gamemap;
  sqact = sqact}


let rec main_helper file_name =
 try
    let open Yojson.Basic in
    let json = from_file file_name in
    let init = init_game json in
    let () = print_endline "Welcome to the Life of a CS Major"; in
    let () = print_endline ""; in repl init
  with
    | Illegal -> let () = print_endline "Please enter a valid game file";
      in main_helper (read_line ())
    | _ -> let () = print_string "This is an invalid game file.";
      print_endline "It does not fit the schema. Please enter a valid json file";
      print_string "> ";
      in main_helper (read_line ())


let main file_name =
   main_helper file_name