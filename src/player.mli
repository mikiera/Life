(*
 * player.mli
 *)

(* A [Player] holds the information about one player in the game *)
module type Player = sig

  (* The type of the player's description *)
  type description

  (* The type of a player *)
  type player

  (* [getID p] returns the ID of the player *)
  val getID : player -> int

  (* [getNickname p] returns the name of the player *)
  val getNickname : player -> string

  (* [getDescription p] returns the description of the player *)
  val getDescription : player -> description

  (* [getHistory p] returns the history of the player *)
  val getHistory : player -> string

  (* [getCollege p] returns the college of the player *)
  val getCollege : player -> string

  (* [getCourse p] returns the course the player is taking *)
  val getCourse : player -> string

  (* [getAdvisor p] returns the player's faculty advisor *)
  val getAdvisor : player -> string

  (* [getPoints p] returns the number of currency points that the player has *)
  val getPoints : player -> int

  (* [getKarma p] returns the number of karma points the player has *)
  val getKarma : player -> int

  (* [getFuture p] returns the player's place of retirement *)
  val getFuture : player -> string

  (* Methods that alter [Player] *)
  (* [addNickname p s] adds a nickname to the player *)
  val addNickname : player -> string -> player

  (* [changeDescription p] changes the description of the player *)
  val changeDescription : player -> description -> player

  (* [addHistory p s] adds another line of history to the player *)
  val addHistory : player -> string -> player

  (* [changeCollege p s] changes the college the player is in *)
  val changeCollege : player -> string -> player

  (* [changeCourse p s] changes the course the player is taking *)
  val changeCourse : player -> string -> player

  (* [changeAdvisor p s] changes the player's faculty advisor *)
  val changeAdvisor : player -> string -> player

  (* [changePoints p i] changes the number of currency points the player has *)
  val changePoints : player -> int -> player

  (* [changeKarma p i] changes the number of karma points the player has *)
  val changeKarma : player -> int -> player

  (* [changeFuture p s] changes the place of the player is retiring at *)
  val changeFuture : player -> string -> player

end