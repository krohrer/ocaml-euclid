(* Kaspar Rohrer, Die Mär  2 03:43:12 CET 2010 *)

(*----------------------------------------------------------------------------*)

type t
type vec = Vector2D.t
type point = Point2D.t

(*------------------------------------*)

val make : point -> point -> t
val make' : minimum:point -> maximum:point -> t

val make_hull : point list -> t
val make_centered : point -> size:vec -> t

(*------------------------------------*)

val empty : t 

(*------------------------------------*)

val x : t -> Interval.t
val y : t -> Interval.t
  
val minimum : t -> point
val maximum : t -> point

val center : t -> point
val size : t -> vec

(*------------------------------------*)
  
val to_string : t -> string

(*------------------------------------*)

val is_empty : t -> bool

(*------------------------------------*)

val insert_point : t -> point -> unit

val add_point : t -> point -> t
val add_points : t -> point list -> t

val clamp_point : t -> point -> point

val classify_point : t -> point -> Flt.fcmp
val contains_point : t -> point -> bool

(*------------------------------------*)

val contains : t -> t -> bool
val intersects : t -> t -> bool
  
val clamp : t -> t -> t
val union : t -> t -> t

(*----------------------------------------------------------------------------*)
