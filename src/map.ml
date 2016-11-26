(* map.ml *)

module type Map = struct
  type location

  type action

  let moveforward : location -> location =
    failwith "Unimplemented"
end

