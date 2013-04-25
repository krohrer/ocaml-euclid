(* $Id$ *)

(*----------------------------------------------------------------------------*)

type t = Vector3D.t
type transform = Transform3D.t

(*------------------------------------*)

val to_string : t -> string

(*------------------------------------*)

val is_equal : eps:float -> t -> t -> bool

val distance_to : t -> t -> float

val transform : transform -> t -> t
val inverse_transform : transform -> t -> t

(*----------------------------------------------------------------------------*)
