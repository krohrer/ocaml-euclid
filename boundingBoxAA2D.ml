(*----------------------------------------------------------------------------*)

module V = Vector2D
module P = Point2D

type vec = V.t
type point = P.t

type t = { mutable minimum : point;
	   mutable maximum : point }

(*------------------------------------*)

let make p1 p2 =
  assert false

let make_hull ps =
  assert false

let make_centered c ~size =
  assert false

(*------------------------------------*)

let empty = make
  V.of

(*------------------------------------*)

let x b = Inverval.of_lower_and_upper (V.x (minimum b)) (V.x (maximum b))
val y b = Interval.of_lower_and_upper (V.y (minimum b)) (V.y (maximum b))
  
let minimum b = b.minimum
let maximum b = b.maximum

let center b = V.mid (minimum b, maximum b)
let size b = V.sub (maximum b) (minimum b)

(*------------------------------------*)
  
let to_string b =
  Printf.sprintf
    "AABB[min=%s, max= %s"
    (V.to_string (minimum b))
    (V.to_string (maximum b))

(*------------------------------------*)

let is_empty b =
  b == empty

(*------------------------------------*)

val contains : t -> t -> bool
val intersects : t -> t -> bool
  
val clamp : t -> t -> t
val union : t -> t -> t

val add_point : t -> point -> t
val add_points : t -> point list -> t

val clamp_point : t -> point -> point

val classify_point : t -> point -> Scalar.fcmp
val contains_point : t -> point -> bool

(*----------------------------------------------------------------------------*)
