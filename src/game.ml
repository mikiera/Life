(* game.ml *)

module type Game = struct
  type turn

  type playerloc

  type gamestate

  let spinner : int list -> int =
    failwith "Unimplemented"

  let play : string -> gamestate -> gamestate =
    failwith "Unimplemented"

  let repl : gamestate -> gamestate =
    failwith "Unimplemented"
end


let main file_name =
  print_newline ("The name of the file is supposedly: " ^ file_name)