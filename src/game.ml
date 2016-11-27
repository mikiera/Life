(* game.ml *)

module Game : sig
  type turn

  type playerloc

  type gamestate

  val spinner: int list -> int

  val play: string -> gamestate -> gamestate

  val repl:  gamestate -> gamestate
end =
struct
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