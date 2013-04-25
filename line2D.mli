(* $Id$ *)

(*----------------------------------------------------------------------------*)

type t
type vec = Vector2D.t
type point =  Point2D.t
type transform = Transform2D.t

(*------------------------------------*)

val make : point -> point -> t
val make' : anchor1:point -> anchor2:point -> t

val make_with_direction : point -> vec -> t
val make_with_direction' : anchor:point -> dir:vec -> t

(*------------------------------------*)

val anchor1 : t -> point
val anchor2 : t -> point
val dir : t -> vec
val inverted_dir : t -> vec

(*------------------------------------*)

val to_string : t -> string

(*------------------------------------*)

val invert : t -> t

(* val distance_to_point : t -> point -> float *)

(* val is_coincident : eps:float -> t -> t -> bool *)

val is_parallel : eps:float -> t -> t -> bool
val is_anti_parallel : eps:float -> t -> t -> bool

val transform : transform -> t -> t
val inverse_transform : transform -> t -> t

(*----------------------------------------------------------------------------*)
