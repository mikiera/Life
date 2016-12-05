(* main.ml
 * Like the main.ml file in A2, this file begins the game and prompts the user
 * to input a json file.
 *)

let () =
  ANSITerminal.(print_string [ANSITerminal.red]
    (" __          __  _                            _           _____                      _ _ _
 \\ \\        / / | |                          | |         / ____|                    | | | |
  \\ \\  /\\  / /__| | ___ ___  _ __ ___   ___  | |_ ___   | |     ___  _ __ _ __   ___| | | |
   \\ \\/  \\/ / _ \\ |/ __/ _ \\| '_ ` _ \\ / _ \\ | __/ _ \\  | |    / _ \\| '__| '_ \\ / _ \\ | | |
    \\  /\\  /  __/ | (_| (_) | | | | | |  __/ | || (_) | | |___| (_) | |  | | | |  __/ | |_|
     \\/  \\/ \\___|_|\\___\\___/|_| |_| |_|\\___|  \\__\\___/   \\_____\\___/|_|  |_| |_|\\___|_|_(_)"
    ^ "\n\nWelcome to Gates Hall (of Hell).\n"
    ^ "You are a prospective Computer Science major at Cornell University!\n"
    ^ "You code die here.. or change the world.. ARE YOU READY?\n\n"));
  print_endline ("Please enter the name of the course roster you want to load.\n"
                ^ "Choose short.json for a snippet of the game or life.json for a full version");
  print_string  ">>> ";
  let file_name = read_line () in
  Game.main file_name