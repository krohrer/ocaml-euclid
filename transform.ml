(*----------------------------------------------------------------------------*)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

type ('a,'m,'n) t = {
  translation : ('a,'m) Vector.t;
  linear : ('a,'m,'n) Matrix.t;
  mutable inverse : ('a,'m,'n) t option
}

open Core
module V = Vector
module M = Matrix

(*----------------------------------------------------------------------------*)

let force_inverse tf =
  match tf.inverse with
      None ->
	let inv_linear = M.inverted tf.linear in
	let inv_translation = M.mulv inv_linear (V.neg tf.translation) in
	let inverse = {
	  linear = inv_linear;
	  translation = inv_translation;
	  inverse = Some tf
	}
	in
	  tf.inverse <- Some inverse;
	  inverse
    | Some inverse ->
	inverse

let make dm dn =
  let linear =
    let l = M.make ~init:0. dm dn in
    let ndiag = min (N.int dm) (N.int dn) in
      for i = 0 to ndiag-1 do
	M.set' l i i 1.
      done;
      M.__absolve__ l
  in
  let translation = V.make ~init:0. dm in
    let rec tf = {
      translation = translation;
      linear = linear;
      inverse = None
    } in
      tf

let make_from ~linear ~translation = {
  translation = translation;
  linear = linear;
  inverse = None
}

let identity' tf =
  V.zero' tf.translation;
  M.identity' tf.linear

let identity d =
  {
    translation = V.zero d;
    linear = M.identity d;
    inverse = None
  }

(*------------------------------------*)

let __absolve__ (tf : (_,'m,'n) t) : (_,'m,'n) t = Obj.magic tf
let __magic__ (tf : (_,'m,'n) t) : (_,'m,'n) t = Obj.magic tf

(*------------------------------------*)

let source_dimension tf =
  M.column_dimension tf.linear

let target_dimension tf =
  M.row_dimension tf.linear

(*------------------------------------*)

let inverted tf =
  force_inverse tf

let invert' tf dst = 
  let inverse = force_inverse tf in
    V.copy' inverse.translation dst.translation;
    M.copy' inverse.linear dst.linear;
    V.copy' tf.translation dst.translation;
    M.copy' tf.linear dst.linear

(*------------------------------------*)

let get_translation' tf dst =
  V.copy' tf.translation dst

let set_translation' tf translation =
  V.copy' translation tf.translation;
  tf.inverse <- None

(*------------------------------------*)

let concatenated tf1 tf2 =
  let l1 = tf1.linear and
      t1 = tf1.translation and
      l2 = tf2.linear and
      t2 = tf2.translation
  in
  let l' = M.mulm l2 l1 in
  let t' = M.mulv l2 t1 in
    V.add' t' t2 t';
    make_from ~linear:l' ~translation:(V.__absolve__ t')

let concatenate' tf1 tf2 dst =
  let l1 = tf1.linear and
      t1 = tf1.translation and
      l2 = tf2.linear and
      t2 = tf2.translation
  in
    M.mulm' l2 l1 dst.linear;
    M.mulv' l2 t1 dst.translation;
    V.add' dst.translation t2 dst.translation

(*------------------------------------*)

let get_linear' tf dst =
  M.copy' tf.linear dst;
  tf.inverse <- None

let set_linear' tf linear =
  M.copy' linear tf.linear;
  tf.inverse <- None
  
(*------------------------------------*)

let translation tf = tf.translation
let linear tf = tf.linear

(*----------------------------------------------------------------------------*)

let transform_point' tf p dst =
  M.mulv' (linear tf) p dst;
  V.add' dst (translation tf) dst

let transform_vector' tf v dst =
  M.mulv' (linear tf) v dst

let transform_normal' tf n dst =
  let inv = force_inverse tf in
    M.vmul' n (linear inv) dst

let inv_transform_point' tf p dst =
  let inv = force_inverse tf in
    M.mulv' (linear inv) p dst;
    V.add' dst (translation inv) dst

let inv_transform_vector' tf v dst =
  let inv = force_inverse tf in
    M.mulv' (linear inv) v dst

let inv_transform_normal' tf n dst =
  M.vmul' n (linear tf) dst

(*----------------------------------------------------------------------------*)

let transformed_point tf p =
  let p' = M.mulv (linear tf) p in
    V.add' p' (translation tf) p';
    V.__absolve__ p'

let transformed_vector tf v =
  M.mulv (linear tf) v

let transformed_normal tf n =
  let inv = force_inverse tf in
    M.vmul n (linear inv)

let inv_transformed_point tf p =
  let inv = force_inverse tf in
  let p' = M.mulv (linear inv) p in
    V.add' p' (translation inv) p';
    V.__absolve__ p'

let inv_transformed_vector tf v =
  let inv = force_inverse tf in
    M.mulv (linear inv) v

let inv_transformed_normal tf n =
  M.vmul n (linear tf)

(*----------------------------------------------------------------------------*)

let is_equal ~eps tf1 tf2 =
  M.is_equal ~eps tf1.linear tf2.linear
  && V.is_equal ~eps tf1.translation tf2.translation

(*------------------------------------*)

open Format
open Hopp

let print fmt tf =
  pp_open_lbox fmt "{";
  pp_print_labeled_field "linear" M.print fmt tf.linear;
  pp_print_separator fmt ";";
  pp_print_labeled_field "translation" V.print fmt tf.translation;
  pp_print_separator fmt ";";
  pp_print_labeled_field "inverse" (pp_print_option pp_print_abstract) fmt tf.inverse;
  pp_close_lbox fmt "}"

let to_string tf = pp_make_to_string print tf

(*----------------------------------------------------------------------------*)


open OUnit
open Printf

let test_equal msg a b =
  assert_equal
    ~msg
    ~cmp:(is_equal ~eps:Scalar.delta)
    ~printer:to_string
    (__magic__ a) b

let identity_suite m = "identity, transform*, inv_transform*" >:: fun () ->
  let r = V.random_unit m in
  let id = identity m in
  let f (msg,f,tf,a,r) =
    V.test_equal msg (f tf a) r
  in
    List.iter f [
      "Id(p)=p", transformed_point, id, r, r;
      "Id(v)=v", transformed_vector, id, r, r;
      "Id(n)=n", transformed_normal, id, r, r;
      "Id^-1(p)=p", inv_transformed_point, id, r, r;
      "Id^-1(v)=v", inv_transformed_vector, id, r, r;
      "Id^-1(n)=n", inv_transformed_normal, id, r, r;
    ]

let inverted_suite m = "random, inverted, concatenated, identity" >:: fun () ->
  let linear = M.random_invertible m in
  let translation = V.random_gaussian m in
  let tf = make_from ~linear ~translation in
    test_equal "T^-1*T=Id" (concatenated (inverted tf) tf) (identity m);
    test_equal "T*T^-1=Id" (identity m) (concatenated (inverted tf) tf);
    ()

let unit_test_for_dim m =
  (sprintf "Dim %d" (N.int m)) >::: [
    inverted_suite m;
    identity_suite m
  ]

let unit_test =
  "ATransform" >::: [
    unit_test_for_dim N._1;
    unit_test_for_dim N._2;
    unit_test_for_dim N._3;
    unit_test_for_dim N._4
  ];

(*----------------------------------------------------------------------------*)

