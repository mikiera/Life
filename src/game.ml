(* game.ml *)
open Gamemap
open Player
open Random

module AT = ANSITerminal

exception Illegal

(* equivalent to player id, 1-indexed *)
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

type direction = Left | Right

type playerid = int

type player_loc_info = {mutable loc: location; mutable dir: direction}

type playerloc = playerid * player_loc_info

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

type gamestate = {turn: playerid;
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

(* constant: color of "choice" text *)
let ccol = AT.red

(* [get_pcol id] gets the color for player with given id *)
let get_pcol id = List.nth [AT.blue; AT.green; AT.magenta] (id mod 3)

(* [print_choice color descrip choices] will print the description in color
 * If the user's input matches a  *)
let rec print_choice color descrip choices =
  let () = AT.print_string [color] (descrip ^ "\n> ") in
  let result = read_line () in
  if (List.mem result choices) then result
  else (print_choice color descrip choices)

let cmd_checker c =
  let a = String.lowercase_ascii (String.trim c) in a

let rec find_player_by_id player_list player_id =
  match player_list with
  | [] -> failwith "this player id is not in the game"
  | h::t -> if (Player.getID h) = player_id then h
            else find_player_by_id t player_id

let rec find_loc_by_sid (locations:location list) (square_id:square):location =
  match locations with
  | [] -> failwith "this square id is not in the game"
  | h::t -> if h.id = square_id then h else find_loc_by_sid t square_id

let change_dir gamestate choice playerid =
    let player_loc_info = List.assoc playerid gamestate.playermap in
    player_loc_info.dir <- choice

let move_one_step gamestate playerid =
    let player_loc_info = List.assoc playerid gamestate.playermap in
    let current_dir = player_loc_info.dir in
    let current_loc = player_loc_info.loc in
    let map = gamestate.gamemap in
    if ((current_dir = Right && current_loc.right <> Null)
        || current_loc.left = Null)
      then (let next_square = current_loc.right in
        player_loc_info.loc <- (find_loc_by_sid map next_square); gamestate)
    else if (current_dir = Left && current_loc.left <> Null)
      then (let next_square = current_loc.left in
        player_loc_info.loc <- (find_loc_by_sid map next_square); gamestate)
    else gamestate

let change_pk (gs:gamestate) (pid:playerid) (action:action):Player.player =
    let player = find_player_by_id gs.players pid in
    ignore((Player.changePoints) player action.points);
    (Player.changeKarma) player action.karma

let rec move_multi_step gamestate playerid n =
    if n = 0 then (let l_info = List.assoc playerid gamestate.playermap in
                   let current_square = l_info.loc.id in
                   let action = List.assoc current_square gamestate.sqact in
                   ignore(change_pk gamestate playerid action))
    else if n > 0 then (let l_info = List.assoc playerid gamestate.playermap in
      if (l_info.loc.left = Null && l_info.loc.right = Null)
      then (let a = List.assoc l_info.loc.id gamestate.sqact in
            ignore(change_pk gamestate playerid a);
            print_endline "You have successfully graduated from the 3110 Life.
            Enter quit to exit the game."; ())
      else (move_multi_step (move_one_step gamestate playerid) playerid (n-1)))
    else failwith "Number of steps can't be negative"

let rec play (cmd : string) (gamestate : gamestate) (turn : int) : gamestate =
  let player = List.nth gamestate.players (turn - 1) in
  if (cmd = "p" || cmd = "points") then (print_endline (string_of_int
    (Player.getPoints player)); repl gamestate turn; gamestate)
  else if (cmd = "h" || cmd = "history") then (print_endline (Player.getHistory
    player); repl gamestate turn; gamestate)
  else if (cmd = "a" || cmd = "advisor") then (print_endline (Player.getAdvisor
    player); repl gamestate turn; gamestate)
  else if (cmd = "c" || cmd = "courses") then (print_endline (Player.getCourse
    player); repl gamestate turn; gamestate)
  else if (cmd = "co" || cmd = "college") then (print_endline (Player.getCollege
    player); repl gamestate turn; gamestate)
  else if (cmd = "n" || cmd = "name") then (print_endline (Player.getNickname
    player); repl gamestate turn; gamestate)
  else if (cmd = "spin") then let spin = ((Random.int 4) + 1) in gamestate
  else if (cmd = "help") then (print_endline ("p/points:      check your total points");
    print_endline ("a/advisor:     see your advisor");
    print_endline ("c/courses:     see your courses");
    print_endline ("co/college:    see your college");
    print_endline ("n/name:        see your nickname");
    print_endline ("spin:          spin the wheel and try your luck!");
    print_endline ("help:          see a list of commands available");
    repl gamestate turn; gamestate)
  else raise Illegal

and repl (state : gamestate) (turn : int) : unit =
  try
    let player = List.nth state.players (turn - 1) in
    let () = AT.print_string [get_pcol turn] ("It is " ^
      (Player.getNickname player) ^ "'s turn. Please enter a command.\n>>> ") in
    let cmd = read_line () in
    let check_cmd = cmd_checker cmd in
    if (check_cmd = "quit" || check_cmd = "exit" || check_cmd = "q") then ()
    else
      let new_gs = play check_cmd state turn in
      let new_turn = (turn mod (List.length state.players)) + 1 in
      repl new_gs new_turn
  with
    | _ -> AT.print_string [get_pcol turn]
      "Invalid command. Please try again.\n"; repl state turn


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
    let playerloclist = ref [] in
    let ailist = ref [] in
    let () = for id = 1 to num_players do
      let player = Player.createPlayer id in
      let () = AT.print_string [get_pcol id]
      ("Hello Player " ^ (string_of_int id) ^ ". What is your name?\n> " ) in
      let name = read_line () in
      let named_player = Player.addNickname player name in
      let final_player = Player.changePoints named_player state.start_points in
      let aimsg = "Will this player be a human player? (Y/N)" in
      let human = print_choice (get_pcol id) aimsg ["Y"; "N"] in
      let () = if (human = "N") then ailist := (!ailist @ [id]) in
      let locobj = find_loc_by_sid state.gamemap (Square state.start) in
      let playerloc = { loc=locobj; dir=Right } in
      playerlist := (!playerlist @ [final_player]);
      playerloclist := (!playerloclist @ [(id, playerloc)])
    done in
    { state with players=(!playerlist); playermap=(!playerloclist) }
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


let rec main_helper (file_name : string) =
  try
    let json = Yojson.Basic.from_file file_name in
    let gamestate1 = init_game json in
    let gamestate2 = setup_players gamestate1 in
    repl gamestate2 1
  with
    | Yojson.Json_error _ -> let () = print_endline ("Invalid json file. Please"
      ^ " try again"); print_string ">>>"; in (main_helper (read_line ()))
    | _ -> let () = print_endline "Invalid input. Please try again";
      print_string ">>> "; in (main_helper (read_line ()))



let main file_name =
   main_helper file_name