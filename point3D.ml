(* $Id$ *)

(*----------------------------------------------------------------------------*)

module V = Vector3D
module T = Transform3D

type t = V.t
type transform = T.t

(*------------------------------------*)

let to_string v = V.to_string v

(*------------------------------------*)

let is_equal ~eps p o =
  V.is_equal ~eps p o

let distance_to p o =
  V.norm (V.sub o p)

let transform t p =
  T.transform_point t p

let inverse_transform t p =
  T.inverse_transform_point t p

(*----------------------------------------------------------------------------*)
