(* $Id$ *)

(*----------------------------------------------------------------------------*)

module V = Vector2D
module P = Point2D
module L = Line2D
module Coeffs = Vector3D
module M = Matrix2D
module T = Transform2D

type vec = V.t
type point = P.t
type line = L.t
type transform = T.t

type coeffs = Coeffs.t
type t = coeffs

(*------------------------------------*)

let cx cs = Coeffs.x cs
let cy cs = Coeffs.y cs
let cd cs = Coeffs.z cs

(*------------------------------------*)

let cd_of_anchored n a =
  -. V.dot n a

let coeffs_of_anchored n a =
  let z = cd_of_anchored n a in
    Coeffs.of_2D ~z n

let anchor_of_coeffs cs =
  let d = cd cs in
    V.make
      (-. d *. (cx cs))
      (-. d *. (cy cs))

let normal_of_coeffs cs =
  Coeffs.to_2D cs

(*------------------------------------*)

let normal cs =
  normal_of_coeffs cs

let anchor cs =
  anchor_of_coeffs cs

(*------------------------------------*)

let make x y d =
  Coeffs.normalize (Coeffs.make x y d)

let make' ?(x=0.) ?(y=0.) ?(d=0.) () =
  make x y d

let make_with_normal n d =
  Coeffs.normalize (Coeffs.of_2D ~z:d n)

let make_with_normal' ~n ~d =
  make_with_normal n d

let make_anchored n a =
  Coeffs.normalize (coeffs_of_anchored n a)

let make_anchored' ~n ~a =
  make_anchored n a

(*------------------------------------*)

let of_coefficients cs =
  Coeffs.normalize cs

let to_coefficients h =
    h

let to_string h =
  Printf.sprintf "[x*%f + y*%f + %f == 0]" (cx h) (cy h) (cd h)

(*------------------------------------*)

let point_offset h p =
  (cx h) *. (V.x p) +. (cy h) *. (V.y p) +. (cd h)

let classify_point ~eps h p =
  Scalar.compare_with_zero ~eps (point_offset h p)
    
let project_point h p =
  let dp =  point_offset h p in
    V.sub p (V.mul1 (normal h) dp)
      
let mirror_point h p =
  let dp = point_offset h p in
    V.sub p (V.mul1 (normal h) (2. *. dp))
      
let intersect2 h1 h2 =
  (*

    Find x, y such that
    
    cx1 * x + cy1 * y + cd1 = 0
      &&
    cx2 * x + cy2 * y + cd2 = 0

    A solution can be found by solving:

    |cx1 cy1|   |x|   |- cd1|
    |cx2 cy2| * |y| = |- cd2|

  *)
  let cx1, cy1, cd1 = cx h1, cy h1, cd h1 in
  let cx2, cy2, cd2 = cx h2, cy h2, cd h2 in
  let coeffs = M.make
    cx1 cy1
    cx2 cy2
  in
  let dneg = V.make
    (-.cd1)
    (-.cd2)
  in
    M.mulv (M.invert coeffs) dneg

let intersect_line h l =
  (*
    Find p such that

    p element of H && element of L

    A solution can be found by calculating:

    (d1*p1 - d2*p2) / (d1-d2)
  *)
  let a1 = L.anchor1 l in
  let a2 = L.anchor2 l in
  let d1 = point_offset h a1 in
  let d2 = point_offset h a2 in
    V.div1
      (V.sub
	 (V.mul1 a1 d2)
	 (V.mul1 a2 d1))
      (d2 -. d1)

let invert hs =
  Coeffs.neg hs

(*----------------------------------------------------------------------------*)
