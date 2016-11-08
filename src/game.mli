 (*
 * game.mli
 *)


 (* A Game holds the gamestate and is the main module for playing the game. The
 * module is what plays the game and modifies and holds the game state which
 * involves a mapping of active players and their locations and whose turn it
 * it *)
 module type Game = sig

 (* represents which player's turn it currently is *)
 type turn

 (* represents a player and their location *)
 type playerloc

 (* represents the game state. It holds list of active players, list of players
 * and their locations, whose turn it is and also holds the list of cards *)
 type gamestate

 (* [spinner] is a function for spinner. it tells the player how many spaces
  * they can move  *)
 val spinner: int list -> int

 (* [play] translates the user commands and returns the new gamestate *)
 val play: string -> gamestate -> gamestate

 (* [repl] is the repl for the main game. this function plays the game and
  * prompts the users for commands*)
 val repl:  gamestate -> gamestate

end
