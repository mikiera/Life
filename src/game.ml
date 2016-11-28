open Player

exception Illegal

module Game : sig
  type turn

  type playerloc

  type gamestate

  val spinner: int list -> int

  val play: string -> gamestate -> gamestate

  val repl:  gamestate -> gamestate
end =
struct
  type turn = int

  type playerloc = unit

  type gamestate = unit

  let player_lst = []

  let spinner (list_nums : int list) : int =
    failwith "Unimplemented"

  let play (cmd : string) (gamestate : gamestate) : gamestate =
  if (cmd = "p" || cmd = "points") then (print_endline (Player.getPoints (List.nth (player_lst) turn)); gamestate)
  else if (cmd = "h" || cmd = "history") then (print_endline (Player.getHistory (List.nth (player_lst) turn)); gamestate)
  else if (cmd = "a" || cmd = "advisor") then (print_endline (Player.getAdvisor (List.nth (player_lst) turn)); gamestate)
  else if (cmd = "c" || cmd = "course") then (print_endline (Player.getCourse (List.nth (player_lst) turn)); gamestate)
  else if (cmd = "co" || cmd = "college") then (print_endline (Player.getCollege (List.nth (player_lst) turn)); gamestate)
  else if (cmd = "n" || cmd = "name") then (print_endline (Player.getNickname (List.nth (player_lst) turn)); gamestate)
  else if (cmd = "spin") then failwith "Unimplemented"
  else if (cmd = "help") then failwtih "Unimplemented"
  else if (cmd = "Choice 1") then failwith "Unimplemented"
  else if (cmd = "Choice 2") then failwith "Unimplemented"
  else raise Illegal

let cmd_checker c =
  let a = String.lowercase_ascii (String.trim c) in a

  let rec repl (state : gamestate) : gamestate =
  print_string  "> ";
  let c = read_line() in
  let a = cmd_checker c in
  if (a = "quit" || a = "exit" || a = "q") then ()
  else try(let new_gs = play (cmd_checker c) state in repl new_gs) with
  |Illegal -> print_endline "Invalid command. Please try again."; repl gs
end


let main file_name =
  print_endline "Entering main function";
  try (let j = init_state (Yojson.Basic.from_file file_name) in repl j) with
  |Yojson.Json_error _-> print_endline "Invalid file"; exit 0
  |Sys_error _ -> print_endline "Invalid input"; exit 0
  |_ -> print_endline "Try again"; exit 0