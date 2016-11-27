module Player : sig
  type player
  val createPlayer: int -> player
  val getID : player -> int
  val getNickname : player -> string
  val getHistory : player -> string
  val getCollege : player -> string
  val getCourse : player -> string
  val getAdvisor : player -> string
  val getPoints : player -> int
  val getKarma : player -> int
  val getFuture : player -> string
  val addNickname : player -> string -> player
  val addHistory : player -> string -> player
  val changeCollege : player -> string -> player
  val changeCourse : player -> string -> player
  val changeAdvisor : player -> string -> player
  val changePoints : player -> int -> player
  val changeKarma : player -> int -> player
  val changeFuture : player -> string -> player
end =

struct
  type player = {id:int; nickname: string ref; history: string ref;
                  college: string ref; course: string ref;
                  advisor: string ref; points: int ref; karma: int ref;
                  future: string ref}
  let createPlayer n = {id = n; nickname = ref ""; history = ref ""; college = ref ""; course = ref "";
                        advisor = ref ""; points = ref 0; karma = ref 0; future = ref ""}
  let getID p = p.id
  let getNickname p = !(p.nickname)
  let getHistory p = !(p.history)
  let getCollege p = !(p.college)
  let getCourse p = !(p.course)
  let getAdvisor p = !(p.advisor)
  let getPoints p = !(p.points)
  let getKarma p = !(p.karma)
  let getFuture p = !(p.future)
  let addNickname p n = p.nickname := n; p
  let addHistory p h = p.history := !(p.history)^h; p
  let changeCollege p c = p.college := c; p
  let changeCourse p c = p.course := c; p
  let changeAdvisor p a = p.advisor:= a; p
  let changePoints p n = p.points := !(p.points) + n; p
  let changeKarma p k = p.karma := !(p.karma) + k; p
  let changeFuture p r = p.future:= r; p
end