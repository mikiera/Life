module type Player = sig
  type history
  type player
  val getID : player -> int
  val getNickname : player -> string
  val getHistory : player -> string
  val getCollege : player -> string
  val getCourse : player -> string
  val getAdvisor : player -> string
  val getPoints : player -> int
  val getKarma : player -> int
  val getRetirement : player -> string
  val addNickname : player -> string -> player
  val addHistory : player -> string -> player
  val changeCollege : player -> string -> player
  val changeCourse : player -> string -> player
  val changeAdvisor : player -> string -> player
  val changePoints : player -> int -> player
  val changeKarma : player -> int -> player
  val changeRetirement : player -> string -> player
end

module Player: Player = struct
  type history = string ref
  type player = {id:int; nickname: string ref; history: history;
                  college: string ref; course: string list ref;
                  advisor: string ref; points: int ref; karma: int ref;
                  retirement: string ref}
  let getID p = p.id
  let getNickname p = !(p.nickname)
  let getHistory p = !(p.history)
  let getCollege p = !(p.college)
  let getCourse p = failwith "Unimplemented" (*!(p.course)*)
  let getAdvisor p = !(p.advisor)
  let getPoints p = !(p.points)
  let getKarma p = !(p.karma)
  let getRetirement p = !(p.retirement)
  let addNickname p n = p.nickname := n; p
  let addHistory p h = p.history := !(p.history)^h; p
  let changeCollege p c = p.college := c; p
  let changeCourse p c = failwith "Unimplemented"
  let changeAdvisor p a = p.advisor:= a; p
  let changePoints p n = p.points := !(p.points) + n; p
  let changeKarma p k = p.karma := !(p.karma) + k; p
  let changeRetirement p r = p.retirement:= r; p

end