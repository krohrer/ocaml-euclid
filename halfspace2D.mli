(* $Id$ *)

(*----------------------------------------------------------------------------*)

type t
type vec = Vector2D.t
type point = Point2D.t
type line = Line2D.t
type transform = Transform2D.t
type coeffs = Vector3D.t

(*------------------------------------*)

val make : float -> float -> float -> t
val make' : ?x:float -> ?y:float -> ?d:float -> unit -> t

val make_with_normal : vec -> float -> t
val make_with_normal' : n:vec -> d:float -> t

val make_anchored : vec -> point -> t
val make_anchored' : n:vec -> a:point -> t

(*------------------------------------*)

val cx : t -> float
val cy : t -> float
val cd : t -> float

val normal : t -> vec
val anchor : t -> point

(*------------------------------------*)

val to_coefficients : t -> coeffs
val of_coefficients : coeffs -> t

val to_string : t -> string

(*------------------------------------*)

val point_offset : t -> point -> float

val classify_point : eps:float -> t -> point -> Flt.fcmp
val project_point : t -> point -> point
val mirror_point : t -> point -> point

val intersect2 : t -> t -> point
val intersect_line : t -> line -> point

val invert : t -> t

(*----------------------------------------------------------------------------*)
