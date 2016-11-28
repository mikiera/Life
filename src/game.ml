(* game.ml *)
open Gamemap
open Player


exception Illegal

type turn = int

type location = Gamemap.OurMap.location

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

let spinner (list_nums : int list) : int =
  failwith "Unimplemented"

let play (cmd : string) (gamestate : gamestate) : gamestate =
  failwith "Unimplemented"

let repl (state : gamestate) : gamestate =
  failwith "Unimplemented"

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


let make_loc_list loc =
  let open Yojson.Basic.Util in
  let id = loc |> member "squareid" |> to_int in
  let left = loc |> member "left" |> to_int in
  let right = loc |> member "right" |> to_int in
  {id = id; left = left; right = right}


let parse_action action =
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








