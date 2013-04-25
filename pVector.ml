(* Kaspar Rohrer, Sat Mar  6 02:14:35 CET 2010 *)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

type ('a,'b,'n) t = 'b array

open Core
module A = Array

(*----------------------------------------------------------------------------*)
(** Creation / Initialization *)
(*----------------------------------------------------------------------------*)

let make n a =
  A.make (N.int n) a

let make1 a =
  [|a|]

let make2 a b =
  [|a;b|]

let make3 a b c =
  [|a;b;c|]

let make4 a b c d =
  [|a;b;c;d|]

let fill' a vec =
  let n = A.length vec in
    A.fill vec 0 n a

(*----------------------------------------------------------------------------*)
(** Access *)
(*----------------------------------------------------------------------------*)

let count vec =
  A.length vec

let dim vec = count vec
let dimension vec = N.__num_of_int__ (dim vec)

let get vec i = vec.(i)
let set' vec i a = vec.(i) <- a

(*----------------------------------------------------------------------------*)
(** Conversion and copying *)
(*----------------------------------------------------------------------------*)

let copy_to_array' vec arr =
  let n = dim vec in
    assert (n = A.length arr);
    A.blit vec 0 arr 0 n

let copy_from_array' vec arr =
  let n = dim vec in
    assert (n = A.length arr);
    A.blit arr 0 vec 0 n

let copied vec =
  A.copy vec

let copy' vec dst =
  let n = dim vec in
    A.blit vec 0 dst 0 n

let xfer' vec dst =
  let n = min (dim vec) (dim dst) in
    A.blit vec 0 dst 0 n

(*----------------------------------------------------------------------------*)
(** Representation *)
(*----------------------------------------------------------------------------*)

let of_array n arr = 
  assert (N.int n = A.length arr);
  arr

let to_array vec = vec

let __absolve__ vec = vec

let __magic__ vec = vec

(*----------------------------------------------------------------------------*)
(** Transformation *)
(*----------------------------------------------------------------------------*)

let iter f vec = Array.iter f vec
let fold_left f z vec = Array.fold_left f z vec
let fold_right f vec z = Array.fold_right f vec z
let map f vec = Array.map f vec

let map' f vec dst =
  let n = dim vec in
    assert (n = dim dst);
    for i = 0 to n-1 do
      dst.(i) <- f vec.(i)
    done

(*----------------------------------------------------------------------------*)
(** Printing *)
(*----------------------------------------------------------------------------*)

open Format
open Hopp

let print ?(pr=pp_print_abstract) fmt vec =
  let n = dim vec in
    pp_open_lbox fmt "[";
    for i = 0 to n-1 do
      if i > 0 then begin
	pp_print_string fmt ";";
	pp_print_cut fmt ()
      end;
      pr fmt vec.(i)
    done;
    pp_close_lbox fmt "]"

let to_string ?pr vec =
    pp_make_to_string (print ?pr) vec

(*----------------------------------------------------------------------------*)
(** Unit testing *)
(*----------------------------------------------------------------------------*)

open OUnit

let test_equal ?pr msg u v =
  assert_equal
    ~msg
    ~printer:(to_string ?pr)
    u v

let test_int_equal (msg,u,v) =
  test_equal ~pr:pp_print_int msg u v

let equality_suite = "make, is_equal" >:: fun () ->
  List.iter test_int_equal [
    "1D", make N._1 1, make1 1;
    "2D", make N._2 2, make2 2 2;
    "3D", make N._3 3, make3 3 3 3;
    "4D", make N._4 4, make4 4 4 4 4;
  ]

let set_get_suite = "get, set" >:: fun () ->
  let u = make4 1 2 3 4 in
  let v = copied u in
    test_equal ~pr:pp_print_int "copied" u v;
    assert_equal ~msg:"dim" (dim u) (dim v);
    let n = dim u in
      for i = 0 to n - 1 do
	set' v i (get u i);
      done;
      test_equal ~pr:pp_print_int "get/set" u v;
      () 

let unit_test =
  "PVector" >::: [
    equality_suite;
    set_get_suite;
    "TODO" >:: fun _ -> todo "More assertions!";
  ]
