(* $Id$ *)

(*----------------------------------------------------------------------------*)

module V = Vector2D
module P = Point2D
module T = Transform2D

type vec = V.t
type point = P.t
type transform = T.t

type t = point * point

(*------------------------------------*)

let make a b =
  a, b

let make' ~anchor1 ~anchor2 =
  make anchor1 anchor2

let make_with_direction a dir =
  make a (V.add a dir)

let make_with_direction' ~anchor ~dir =
  make_with_direction anchor dir

(*------------------------------------*)

let anchor1 (a, _) = a
let anchor2 (_, b) = b
let dir (a, b) = V.sub b a
let inverted_dir (a, b) = V.sub a b

(*------------------------------------*)

let to_string (a, b) =
  Printf.sprintf "[%s - %s]" (P.to_string a) (P.to_string b)

(*------------------------------------*)

let invert (a, b) = b, a

let is_parallel ~eps l k =
  V.is_parallel ~eps (dir l) (dir k)

let is_anti_parallel ~eps l k =
  V.is_anti_parallel ~eps (dir l) (dir k)

let transform t (a, b) =
  P.transform t a, P.transform t b

let inverse_transform t (a, b) =
  P.inverse_transform t a, P.inverse_transform t b
  
(*----------------------------------------------------------------------------*)
