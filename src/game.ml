(* game.ml *)
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

type card = {name: string;
             description: string;
             id: int;
             points: int;
             karma: int;
             card_type: actionType}

type playercard = (playerid * card list)

type gamecomp = {courses: card list;
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
                  playercard: playercard list;
                  active_players: int list}

(* constant: game color *)
let gcol = AT.black

(* constant: color of "choice" text *)
let ccol = AT.red

(* [get_pcol id] gets the color for player with given id *)
let get_pcol id = List.nth [AT.blue; AT.green; AT.magenta] (id mod 3)


let cmd_checker c =
  let a = String.lowercase_ascii (String.trim c) in a

(* [print_choice color descrip choices] will print the description in color
 * If the user's input matches a string in list choices *)
let rec print_choice color descrip choices =
  let () = AT.print_string [color] (descrip ^ "\n> ") in
  let result = read_line () in
  let fixedresult = cmd_checker result in
  if (List.mem fixedresult choices) then fixedresult
  else (print_choice color descrip choices)

let rec find_player_by_id player_list player_id =
  match player_list with
  | [] -> failwith "this player id is not in the game"
  | h::t -> if (Player.getID h) = player_id then h
            else find_player_by_id t player_id

let rec find_loc_by_sid (locations:location list) (square_id:square):location =
  match locations with
  | [] -> failwith "this square id is not in the game"
  | h::t -> if h.id = square_id then h else find_loc_by_sid t square_id

let remove_card card gamecomp gamestate =
  let newgamecomp =
    begin match card.card_type with
    | ChoiceC -> let lst = gamecomp.courses in
                let cardlst = List.filter (fun x -> x.id <> card.id) lst in
                {gamecomp with courses = cardlst}
    | ChoiceA -> let lst = gamecomp.advisors in
                 let cardlst = List.filter (fun x -> x.id <> card.id) lst in
                 {gamecomp with advisors = cardlst}
    | ChoiceF -> let lst = gamecomp.future in
                let cardlst = List.filter (fun x -> x.id <> card.id) lst in
                {gamecomp with future = cardlst}
    | ChoiceS -> let lst = gamecomp.summer in
                let cardlst = List.filter (fun x -> x.id <> card.id) lst in
                {gamecomp with summer = cardlst}
    | ChoiceCol -> gamecomp
    | Event -> gamecomp
    end in
  {gamestate with gamecomp = newgamecomp}

let add_card card gamecomp gamestate =
  let newgamecomp =
    begin match card.card_type with
    | ChoiceC -> let lst = gamecomp.courses in
                let cardlst = card :: lst in
                {gamecomp with courses = cardlst}
    | ChoiceA -> let lst = gamecomp.advisors in
                 let cardlst = card :: lst in
                 {gamecomp with advisors = cardlst}
    | ChoiceF -> let lst = gamecomp.future in
                let cardlst = card :: lst in
                {gamecomp with future = cardlst}
    | ChoiceS -> let lst = gamecomp.summer in
                let cardlst = card :: lst in
                {gamecomp with summer = cardlst}
    | ChoiceCol -> gamecomp
    | Event -> gamecomp
    end in
  {gamestate with gamecomp = newgamecomp}


let get_correct_comp actionType gamestate =
  match actionType with
    | ChoiceC -> gamestate.gamecomp.courses
    | ChoiceA -> gamestate.gamecomp.advisors
    | ChoiceF -> gamestate.gamecomp.future
    | ChoiceS -> gamestate.gamecomp.summer
    | ChoiceCol -> failwith "no gamecomp"
    | Event -> failwith "no gamecomp"

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
                   let () = AT.print_string [get_pcol playerid]
                    (action.description ^ "\n") in
                   ignore(change_pk gamestate playerid action); gamestate)
    else if n > 0 then (let l_info = List.assoc playerid gamestate.playermap in
      if (l_info.loc.left = Null && l_info.loc.right = Null)
      then (let a = List.assoc l_info.loc.id gamestate.sqact in
            ignore(change_pk gamestate playerid a);
            print_endline "You have successfully graduated from the 3110 Life. Wait for your friends to join you!";
            let new_turn = gamestate.turn - 1 in
            let active_players = gamestate.active_players in
            let new_active = List.filter (fun x -> (x <> playerid)) active_players in
            {gamestate with active_players = new_active; turn = new_turn})
      else (move_multi_step (move_one_step gamestate playerid) playerid (n-1)))
    else failwith "Number of steps can't be negative"

let rec check_for_fork playerid square gamestate num_step =
  if num_step = 0 then false
  else (let loc = find_loc_by_sid gamestate.gamemap square in
        if loc.right = Null then false
        else if loc.left <> Null && loc.right <> Null then true
        else check_for_fork playerid loc.right gamestate (num_step-1))

(* [get_step_for_choice_event playerid square gamestate num_step] returns a
 * tuple of corrected number of steps for mandatory stops and the actionType *)
 (* starts at loc.right *)
let rec get_step_for_choice_event playerid square gamestate num_step =
  let loc = find_loc_by_sid gamestate.gamemap square in
  let action = List.assoc square gamestate.sqact in
  if num_step = 1 then (num_step, action.actionType)
  else
    if (action.actionType = Event)
      then get_step_for_choice_event playerid loc.right gamestate (num_step -1)
    else (num_step, action.actionType)

(* [handle_fork playerid player_loc_info gamestate step] handles fork events and
 * returns a new gamestate *)
let handle_fork playerid player_loc_info gamestate step =
  let msg =
  "There's a fork in your path. Do you want to turn left or right? (L/R)" in
  let choice = print_choice (get_pcol playerid) msg ["L"; "l"; "R"; "r"]
  in (if choice = "L" || choice = "l" then player_loc_info.dir <- Left
      else player_loc_info.dir <- Right);
  move_multi_step gamestate playerid step

(* [pick_college player gamestate] allows the user to select a college and
 * modifies a player and returns a new gamestate
 *)
let pick_college player gamestate =
  let msg = "To choose Arts and Sciences, type AS. For Engineering, type ENG" in
  let choice = print_choice ccol msg ["AS"; "as"; "As"; "ENG"; "eng"; "Eng"] in
  if (choice = "AS" || choice = "as" || choice = "As")
    then (ignore((Player.changeCollege) player "Arts and Sciences"); gamestate)
  else (ignore((Player.changeCollege) player "Engineering"); gamestate)


let rec create_message_from_cards msg cardlst =
  match cardlst with
    | [] -> msg
    | h :: t -> let desc = h.name in
                let id = string_of_int h.id in
                let newmsg = msg^" "^id^") "^desc in
                create_message_from_cards newmsg t

let rec get_list_of_valid_choices cardlst lst =
  match cardlst with
    | [] -> lst
    | h :: t -> let newlst = (string_of_int h.id) :: lst in
                get_list_of_valid_choices t newlst

let get_start_msg actionType =
  match actionType with
  | ChoiceC ->
  "Choose a course from the following list by typing the course number:"
  | ChoiceA ->
  "Choose an advisor from the following list by typing the advisor number:"
  | ChoiceF ->
  "Determine your future from the following list by typing the corresponding number"
  | ChoiceS ->
  "Choose your summer plans from the following list by typing the corresponding number"
  | _ -> "not a valid actionType"


let rec get_card_by_id id cardlst =
  match cardlst with
    | [] -> failwith "this card is not available"
    | h :: t -> if h.id = id then h else get_card_by_id id t


let rec check_if_player_has_card playercardlst actionType =
  match playercardlst with
    | [] -> ([], playercardlst)
    | h :: t -> if h.card_type=actionType then ([h],t)
                else check_if_player_has_card t actionType

let update_player player actionType card =
  let name = card.name in
  match actionType with
    | ChoiceC -> (Player.changeCourse) player name
    | ChoiceA -> (Player.changeAdvisor) player name
    | ChoiceF -> (Player.changeFuture) player name
    | ChoiceS -> (Player.changeSummerPlans) player name
    | ChoiceCol -> (Player.changeCollege) player name
    | Event -> player

let update_player_card_list playerid playercardlst gamestate newcardlst =
  let noplaylst = List.filter (fun (x,y) -> x <> playerid) playercardlst in
  let newlst = (playerid, newcardlst) :: noplaylst in
  {gamestate with playercard = newlst}


let handle_choice_helper player gamestate actionType =
  let playerid = Player.getID player in
  let cardlst = get_correct_comp actionType gamestate in
  let valid_choices = get_list_of_valid_choices cardlst [] in
  let cardmsg = create_message_from_cards "" cardlst in
  let startmsg = get_start_msg actionType in
  let msg = startmsg^" : "^cardmsg in
  let choice = print_choice ccol msg valid_choices in
  let id = int_of_string choice in
  let newcard = get_card_by_id id cardlst in
  let playercardlst = List.assoc playerid gamestate.playercard in
  let (oldcard, fixpclst) = check_if_player_has_card playercardlst actionType in
  let newpclst = newcard :: fixpclst in
  let gs =
    update_player_card_list playerid gamestate.playercard gamestate newpclst in
  let newgs_remove = remove_card newcard gs.gamecomp gs in
  (ignore (update_player player actionType newcard));
  if oldcard = [] then newgs_remove
  else let addcard = List.hd oldcard in
  add_card addcard newgs_remove.gamecomp newgs_remove


(* [handle_choice playerid gamestate actionType] handles choice
 * events and returns a new gamestate with the corrected components *)
let handle_choice player gamestate actionType : gamestate =
  if (actionType = ChoiceCol) then pick_college player gamestate
  else handle_choice_helper player gamestate actionType

(* [spin_helper gamestate player step] is a helper function that handles spin
 * command *)
let spin_helper gamestate player step =
  let playerid = Player.getID player in
  let player_loc_info = List.assoc playerid gamestate.playermap in
  let (leftover,actionType) =
  get_step_for_choice_event playerid player_loc_info.loc.right gamestate step in
  let newstep = step - leftover + 1 in
  let () = AT.print_string [get_pcol (Player.getID player)]
      ("You have moved " ^ (string_of_int newstep) ^ " steps. Hooray!\n") in
  if (actionType = Event) then
    (if not (check_for_fork playerid player_loc_info.loc.id gamestate newstep)
      then move_multi_step gamestate playerid newstep
    else handle_fork playerid player_loc_info gamestate newstep)
  else
    (if not (check_for_fork playerid player_loc_info.loc.id gamestate newstep)
     then let newgs = move_multi_step gamestate playerid newstep in
        handle_choice player newgs actionType
    else
      let new_gs = handle_fork playerid player_loc_info gamestate newstep in
      handle_choice player new_gs actionType)

let rec play (cmd : string) (gamestate : gamestate) (turn : int) : gamestate =
  let playerid = List.nth gamestate.active_players turn in
  let player = List.nth gamestate.players (playerid - 1) in
  if (cmd = "p" || cmd = "points") then (AT.print_string [get_pcol playerid]
    (string_of_int (Player.getPoints player) ^ "\n"); gamestate)
  else if (cmd = "h" || cmd = "history") then (AT.print_string [get_pcol playerid]
    (Player.getHistory player); gamestate)
  else if (cmd = "a" || cmd = "advisor") then (AT.print_string [get_pcol playerid]
    ((Player.getAdvisor player) ^ "\n"); gamestate)
  else if (cmd = "c" || cmd = "courses") then (AT.print_string [get_pcol playerid]
    ((Player.getCourse player) ^ "\n"); gamestate)
  else if (cmd = "co" || cmd = "college") then (AT.print_string [get_pcol playerid]
    ((Player.getCollege player) ^ "\n"); gamestate)
  else if (cmd = "n" || cmd = "name") then (AT.print_string [get_pcol playerid]
    ((Player.getNickname player) ^ "\n"); gamestate)
  else if (cmd = "spin") then let step = ((Random.int 4) + 1) in
    spin_helper gamestate player step
  else if (cmd = "help") then (
    AT.print_string [get_pcol playerid]
    ("p/points:      check your total points\n");
    AT.print_string [get_pcol playerid]
    ("a/advisor:     see your advisor\n");
    AT.print_string [get_pcol playerid]
    ("c/courses:     see your courses\n");
    AT.print_string [get_pcol playerid]
    ("co/college:    see your college\n");
    AT.print_string [get_pcol playerid]
    ("n/name:        see your nickname\n");
    AT.print_string [get_pcol playerid]
    ("spin:          spin the wheel and try your luck!\n");
    AT.print_string [get_pcol playerid]
    ("help:          see a list of commands available\n");
    gamestate)
  else raise Illegal

and repl (state : gamestate) (turn : int) : unit =
  try
    let playerid = List.nth state.active_players turn in
    let player = List.nth state.players (playerid - 1) in
    let () = AT.print_string [get_pcol playerid] ("It is " ^
      (Player.getNickname player) ^ "'s turn. Please enter a command.\n>>> ") in
    let cmd = read_line () in
    let check_cmd = cmd_checker cmd in
    if (check_cmd = "quit" || check_cmd = "exit" || check_cmd = "q")
    then AT.print_string [gcol] "You have terminated the game.\n"
    else
      let new_gs = play check_cmd state turn in
      let new_turn = if (check_cmd <> "spin") then turn
        else ((turn + 1) mod (List.length state.active_players)) in
      repl new_gs new_turn
  with
    | _ -> AT.print_string [gcol]
      "Invalid command. Please try again.\n"; repl state turn
(*priya can you add end of the game when active player list is empty *)

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
    let activeplayers = ref [] in
    let playercard = ref [] in
    let ailist = ref [] in
    let () = for id = 1 to num_players do
      let player = Player.createPlayer id in
      let () = AT.print_string [get_pcol id]
      ("Hello Player " ^ (string_of_int id) ^ ". What is your name?\n> " ) in
      let name = read_line () in
      let named_player = Player.addNickname player name in
      let final_player = Player.changePoints named_player state.start_points in
      let aimsg = "Will this player be a human player? (Y/N)" in
      let res = print_choice (get_pcol id) aimsg ["Y"; "y"; "N"; "n"] in
      let () = if (res = "N" || res = "n") then ailist := (!ailist @ [id]) in
      let locobj = find_loc_by_sid state.gamemap (Square state.start) in
      let playerloc = { loc=locobj; dir=Right } in
      activeplayers := (!activeplayers @ [id]);
      playerlist := (!playerlist @ [final_player]);
      playerloclist := (!playerloclist @ [(id, playerloc)]);
      playercard := (!playercard @ [(id, [])]);
    done in
    { state with players=(!playerlist); playermap=(!playerloclist);
      active_players=(!activeplayers); playercard = (!playercard) }
  with
    | _ -> setup_players state

let init_game j =
  let open Yojson.Basic.Util in
  let courses = j |> member "courses" |> to_list
    |> List.map (extract_card ChoiceC) in
  let advisors = j |> member "advisors" |> to_list
    |>  List.map (extract_card ChoiceA) in
  let summer = j |> member "summer_plans" |> to_list
    |>  List.map (extract_card ChoiceS) in
  let future = j |> member "future_plans" |> to_list
    |>  List.map (extract_card ChoiceF) in
  let gamecomp = {courses = courses; advisors = advisors;
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
  playercard = [];
  sqact = sqact}


let rec main_helper (file_name : string) =
  try
    let json = Yojson.Basic.from_file file_name in
    let gamestate1 = init_game json in
    let gamestate2 = setup_players gamestate1 in
    repl gamestate2 (gamestate2.turn)
  with
    | Yojson.Json_error _ -> let () = print_endline ("Invalid json file. Please"
      ^ " try again"); print_string ">>>"; in (main_helper (read_line ()))
    | _ -> let () = print_endline "Invalid input. Please try again";
      print_string ">>> "; in (main_helper (read_line ()))


let main file_name =
   main_helper file_name