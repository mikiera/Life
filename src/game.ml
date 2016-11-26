(* game.ml *)

module Game = struct
  type turn = unit

  type playerloc = unit

  type gamestate = unit

  let spinner (list_nums : int list) : int =
    failwith "Unimplemented"

  let play (cmd : string) (gamestate : gamestate) : gamestate =
    failwith "Unimplemented"

  let repl (state : gamestate) : gamestate =
    failwith "Unimplemented"
end


let main file_name =
  print_endline "Entering main function"