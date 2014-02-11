(* $Id$ *)

(*----------------------------------------------------------------------------*)

module V = Vector3D
module P = Point3D
module L = Line3D
module Coeffs = Vector4D
module M = Matrix3D
module T = Transform3D

type vec = V.t
type point = P.t
type line = L.t
type transform = T.t

type coeffs = Coeffs.t
type t = coeffs

(*------------------------------------*)

let cx cs = Coeffs.x cs
let cy cs = Coeffs.y cs
let cz cs = Coeffs.z cs
let cd cs = Coeffs.z cs

(*------------------------------------*)

let cd_of_anchored n a =
  -. V.dot n a

let coeffs_of_anchored n a =
  let w = cd_of_anchored n a in
    Coeffs.of_3D ~w n

let anchor_of_coeffs cs =
  let d = cd cs in
    V.make
      (-. d *. (cx cs))
      (-. d *. (cy cs))
      (-. d *. (cz cs))

let normal_of_coeffs cs =
  Coeffs.to_3D cs

(*------------------------------------*)

let normal cs =
  normal_of_coeffs cs

let anchor cs =
  anchor_of_coeffs cs

(*------------------------------------*)

let make x y z d =
  Coeffs.normalize (Coeffs.make x y z d)

let make' ?(x=0.) ?(y=0.) ?(z=0.) ?(d=0.) () =
  make x y z d

let make_with_normal n d =
  Coeffs.normalize (Coeffs.of_3D ~w:d n)

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
  Printf.sprintf "[x*%f + y*%f + z*%f + %f == 0]" (cx h) (cy h) (cz h) (cd h)

(*------------------------------------*)

let point_offset h p =
  (cx h) *. (V.x p) +. (cy h) *. (V.y p) +. (cz h) *. (V.z p) +. (cd h)

let classify_point ~eps h p =
  Scalar.compare_with_zero ~eps (point_offset h p)
    
let project_point h p =
  let dp =  point_offset h p in
    V.sub p (V.mul1 (normal h) dp)
      
let mirror_point h p =
  let dp = point_offset h p in
    V.sub p (V.mul1 (normal h) (2. *. dp))
      
let intersect3 h1 h2 h3 =
  (*

    Find x, y such that
    
    cx1 * x + cy1 * y + cz1 * z + cd1 = 0
      &&
    cx2 * x + cy2 * y + cz2 * z + cd2 = 0
      &&
    cx3 * x + cy3 * y + cz3 * z + cd3 = 0

    A solution can be found by solving:

    |cx1 cy1 cz1|   |x|   |- cd1|
    |cx2 cy2 cz2| * |y| = |- cd2|
    |cx1 cy1 cz3|   |z|   |- cd3|

  *)
  let cx1, cy1, cz1, cd1 = cx h1, cy h1, cz h1, cd h1 in
  let cx2, cy2, cz2, cd2 = cx h2, cy h2, cz h2, cd h2 in
  let cx3, cy3, cz3, cd3 = cx h3, cy h3, cz h3, cd h3 in
  let coeffs = M.make
    cx1 cy1 cz1
    cx2 cy2 cz2
    cx3 cy3 cz3
  in
  let dneg = V.make
    (-.cd1)
    (-.cd2)
    (-.cd3)
  in
    M.mulv (M.invert coeffs) dneg

let intersect2 h1 h2 =
  let dir = V.cross (normal h1) (normal h2) in
  let h3 = make_with_normal dir 0. in
  let o = intersect3 h1 h2 h3 in
    L.make_with_direction o (V.add o dir)

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

let invert h =
  Coeffs.neg h

(*----------------------------------------------------------------------------*)
