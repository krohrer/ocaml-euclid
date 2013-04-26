(*----------------------------------------------------------------------------*)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

type ('a,'d) t = ('a,'d) Vector.t

open Core
module V = Vector
module M = Matrix

let of_vector v = v
let to_vector v = v

let delta u v =
  V.sub v u

let distance u v =
  V.magnitude (delta u v)
  
let distance_squared u v = 
  V.magnitude_squared (delta u v)

let print =
  V.print

(*----------------------------------------------------------------------------*)
