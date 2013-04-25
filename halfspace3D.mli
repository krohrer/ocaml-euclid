(* $Id$ *)

(*----------------------------------------------------------------------------*)

type t
type vec = Vector3D.t
type point = Point3D.t
type line = Line3D.t
type transform = Transform3D.t
type coeffs = Vector4D.t

(*------------------------------------*)

val make : float -> float -> float -> float -> t
val make' : ?x:float -> ?y:float -> ?z:float -> ?d:float -> unit -> t

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

val intersect2 : t -> t -> line
val intersect3 : t -> t -> t -> point
val intersect_line : t -> line -> point

val invert : t -> t

(*----------------------------------------------------------------------------*)
