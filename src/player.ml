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
  val getSummerPlans : player -> string
  val isHuman : player -> bool
  val addNickname : player -> string -> player
  val addHistory : player -> string -> player
  val changeCollege : player -> string -> player
  val changeCourse : player -> string -> player
  val changeAdvisor : player -> string -> player
  val changePoints : player -> int -> player
  val changeKarma : player -> int -> player
  val changeFuture : player -> string -> player
  val changeSummerPlans : player -> string -> player
  val setMode : player -> bool -> unit
end =

struct
  type player = {id:int; nickname: string ref; history: string ref;
                  college: string ref; course: string ref;
                  advisor: string ref; points: int ref; karma: int ref;
                  future: string ref; summer_plans: string ref; mode: bool ref}
  let createPlayer n = {id = n; nickname = ref ""; history = ref ("ID: " ^ string_of_int(n) ^ "\n"); college = ref ""; course = ref "";
                        advisor = ref ""; points = ref 0; karma = ref 0; future = ref ""; summer_plans = ref ""; mode = ref true}
  let getID p = p.id
  let getNickname p = !(p.nickname)
  let getHistory p = !(p.history)
  let getCollege p = !(p.college)
  let getCourse p = !(p.course)
  let getAdvisor p = !(p.advisor)
  let getPoints p = !(p.points)
  let getKarma p = !(p.karma)
  let getFuture p = !(p.future)
  let getSummerPlans p = !(p.summer_plans)
  let isHuman p = !(p.mode)
  let addHistory p h = p.history := !(p.history)^h; p
  let addNickname p n = p.nickname := n; addHistory p ("Name: " ^ n ^ "\n")
  let changeCollege p c = p.college := c; addHistory p ("College: " ^ c ^ "\n")
  let changeCourse p c = p.course := c; addHistory p ("Course: " ^ c ^ "\n")
  let changeAdvisor p a = p.advisor:= a; addHistory p ("Advisor: " ^ a ^ "\n")
  let changePoints p n = p.points := !(p.points) + n; p
  let changeKarma p k = p.karma := !(p.karma) + k; p
  let changeFuture p r = p.future:= r; p
  let changeSummerPlans p s = p.summer_plans:= s; p
  let setMode p b = p.mode:= b
end