(* main.ml
 * Like the main.ml file in A2, this file begins the game.
 *)

let () =
  ANSITerminal.(print_string [black]
    "\n\nWelcome to the Game of the CS Life.\nYou are a prospective Computer \
    Science major at Cornell University!\n");
  print_endline "Please enter the name of the course roster you want to load.";
  print_string  ">>> ";
  let file_name = read_line () in
  Game.main file_name