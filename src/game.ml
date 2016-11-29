(* game.ml *)
open Gamemap
open Player
open Random

module AT = ANSITerminal

exception Illegal

type turn = int

type square = Null | Square of int

type location = {id: square; left: square; right: square}

type actionType = Event | ChoiceC | ChoiceA | ChoiceS | ChoiceF | ChoiceCol

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
                  gamecomp: gamecomp;
                  players: Player.player list;
                  active_players: int list}

(* constant: game color *)
let gcol = AT.black

(* constant: list of player colors *)
let pcol = [AT.blue; AT.green; AT.magenta]

(* constant: color of "choice" text *)
let ccol = AT.red

(* [get_pcol id] gets the color for player with given id *)
let get_pcol id = List.nth pcol (id mod 3)

(* [print_choice color descrip choices] will print the description in color
 * If the user's input matches a  *)
let rec print_choice color descrip choices =
  let () = AT.print_string [color] (descrip ^ "\n> ") in
  let result = read_line () in
  if (List.mem result choices) then result
  else (print_choice color descrip choices)

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
    | "college" -> ChoiceCol
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

(* [setup_player state] takes a game generated after parsing a game json and
 * initializes it with players *)
let rec setup_players state =
  try
    let () = AT.print_string [gcol] ("How many players are there in the game?"
      ^ " (1-8)\n> ") in
    let num_players = int_of_string (read_line ()) in
    let () = if (num_players < 1 || num_players > 8)
      then failwith "Invalid number of players.\n" in
    let playerlist = ref [] in
    let ailist = ref [] in
    let () = for id = 1 to num_players do
      let player = Player.createPlayer id in
      let () = AT.print_string [get_pcol id]
      ("Hello Player " ^ (string_of_int id) ^ ". What is your name?\n> " ) in
      let name = read_line () in
      let named_player = Player.addNickname player name in
      let aimsg = "Will this player be a human player? (Y/N)" in
      let human = print_choice (get_pcol id) aimsg ["Y"; "N"] in
      let () = if (human = "N") then ailist := (!ailist @ [id]) in
      playerlist := (!playerlist @ [named_player])
    done in
    { state with players=(!playerlist) }
  with
    | _ -> setup_players state

let init_game j =
  let open Yojson.Basic.Util in
  let courses = j |> member "courses" |> to_list
    |> List.map (extract_card COURSE) in
  let advisors = j |> member "advisors" |> to_list
    |>  List.map (extract_card ADVISOR) in
  let summer = j |> member "summer_plans" |> to_list
    |>  List.map (extract_card SUMMER) in
  let future = j |> member "future_plans" |> to_list
    |>  List.map (extract_card FUTURE) in
  let gamecomp = {college = NONE; courses = courses; advisors = advisors;
  summer = summer; future = future } in
  let start = j |> member "start" |> to_int in
  let start_points = j |> member "start_points" |> to_int in
  let gamemap = j |> member "map" |> to_list |> List.map make_loc_list in
  let sqact = j |> member "square_action" |> to_list |> List.map sq_act_list in
  {turn=0; playermap = [];
  gamecomp = gamecomp;
  players = [];
  active_players = [];
  start = start;
  start_points = start_points;
  gamemap = gamemap;
  sqact = sqact}


let rec main_helper file_name =
 try
    let () = print_endline "Welcome to the Life of a CS Major"; in
    let c = cmd_checker file_name in if (c = "quit" || c = "exit" || c = "q") then ()
    else
    let open Yojson.Basic in
    let json = from_file file_name in
    let init = init_game json in
    let setup_people = setup_players init in
    repl setup_people
  with
    | Illegal -> let () = print_endline "Please enter a valid game file";
      in main_helper (read_line ())
    | _ -> let () = print_string "This is an invalid game file.";
      print_endline "It does not fit the schema. Please enter a valid json file";
      print_string "> ";
      in main_helper (read_line ())


let main file_name =
   main_helper file_name