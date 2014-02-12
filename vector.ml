(*----------------------------------------------------------------------------*)

open Core
module Imp = ImpVec

type 'n t = float array

type c1 = [`x]
type c2 = [`x|`y]
type c3 = [`x|`y|`z]
type c4 = [`x|`y|`z|`w]

(*----------------------------------------------------------------------------*)

let init d f =
  let n = N.int d in
  let dst = Imp.alloc ~n in
  Imp.init ~n ~dst f;
  dst

let fill d s =
  let n = N.int d in
  let dst = Imp.alloc ~n in
  Imp.fill ~n ~dst s;
  dst

let make1 x = [|x|]
let make2 x y = [|x;y|]
let make3 x y z = [|x;y;z|]
let make4 x y z w= [|x;y;z;w|]

let size a =
  Array.length a

let dimension a =
  N.__num_of_int__ (size a)

(*------------------------------------*)

let memoize f =
  let s1 = f 1 and
      s2 = f 2 and
      s3 = f 3 and
      s4 = f 4 in
  fun d -> match N.int d with
  | 1 -> s1
  | 2 -> s2
  | 3 -> s3
  | 4 -> s4
  | n -> f n

let zeroN n = Array.make n 0.
let oneN n  = Array.make n 1.
let unityN k n =
  let dst = Imp.alloc ~n in
  Imp.unity ~n ~dst k;
  dst

let zero d = memoize zeroN d
let one d  = memoize oneN  d

let ex d = memoize (unityN 0) d
let ey d = memoize (unityN 1) d
let ez d = memoize (unityN 2) d
let ew d = memoize (unityN 3) d

(*------------------------------------*)

let to_array a = Array.copy a

let from_array a d =
  if N.int d == Array.length a then
    Array.copy a
  else
    invalid_arg "Vector.from_array: mismatched size."

external __repr__ : 'n t -> ImpVec.t = "%identity"

(*------------------------------------*)

(* no assert necessary here because ocaml checks indices *)
let get i v = v.(i)

let x = get 0
let y = get 1
let z = get 2
let w = get 3

(*------------------------------------*)

let purify1 fimp a =
  let n = size a in
  let dst = Imp.alloc ~n in
  fimp ~n ~dst;
  dst

let purify2 fimp a b =
  let n = size a in
  let dst = Imp.alloc ~n in
  fimp ~n ~dst a b;
  dst

let purify1n ~n fimp a =
  let dst = Imp.alloc ~n in
  fimp ~dst a;
  dst

let purify2n ~n fimp a b =
  let dst = Imp.alloc ~n in
  fimp ~dst a b;
  dst

let neg = purify1 Imp.neg
let add = purify2 Imp.add
let sub = purify2 Imp.sub
let mul = purify2 Imp.mul
let div = purify2 Imp.div

let smul s a =
  let n = size a in 
  let dst = Imp.alloc ~n in
  Imp.smul ~n ~dst s a;
  dst

let divs = purify2 Imp.divs
let abs = purify1 Imp.abs
let min = purify2 Imp.min
let max = purify2 Imp.max

let dot a b =
  let n = size a in Imp.dot ~n a b
      
let lensq a =
  let n = size a in Imp.dot ~n a a 

let len a = sqrt (lensq a)

let cross a b =
  let dst = Imp.alloc ~n:3 in
  Imp.cross ~dst a b;
  dst

let normalize = purify1 Imp.normalize
let mid = purify2 Imp.mid
let lerp ~t = purify2 (Imp.lerp ~t)

let minmax a b =
  let n = size a in
  let dmin = Imp.alloc n and dmax = Imp.alloc n in
  Imp.minmax ~n ~dmin ~dmax a b;
  dmin, dmax

let c2i = function
  | `x -> 0
  | `y -> 1
  | `z -> 2
  | `w -> 3

let swizzle2 cx cy =
  purify1n ~n:2 (Imp.swizzle2 (c2i cx) (c2i cy))
let swizzle3 cx cy cz =
  purify1n ~n:3 (Imp.swizzle3 (c2i cx) (c2i cy) (c2i cz))
let swizzle4 cx cy cz cw =
  purify1n ~n:4 (Imp.swizzle4 (c2i cx) (c2i cy) (c2i cz) (c2i cw))

let mask2 cx cy =
  purify2n ~n:2 (Imp.mask2 cx cy)
let mask2 cx cy cz =
  purify2n ~n:2 (Imp.mask3 cx cy cz)
let mask2 cx cy cz cw =
  purify2n ~n:2 (Imp.mask4 cx cy cz cw)

(*------------------------------------*)

let is_equal eps a b =
  a == b || let n = size a in Imp.is_equal eps ~n a b

let is_zero eps a =
  let n = size a in Imp.is_zero eps ~n a

(*------------------------------------*)

let random ?(state=Rnd.default) ?lower ?upper d =
  let n = N.int d in
  let dst = Imp.alloc n in
  let lower = Option.may_default zero d lower in
  let upper = Option.may_default one d upper in
  Imp.random state ~n ~dst ~lower ~upper;
  dst

let random_unit ?(state=Rnd.default) d =
  let n = N.int d in
  let dst = Imp.alloc n in
  Imp.random_unit state ~n ~dst;
  dst

let random_gaussian ?(state=Rnd.default) ?mu ?sigma d =
  let n = N.int d in
  let dst = Imp.alloc n in
  let mu = Option.may_default zero d mu in
  let sigma = Option.may_default one d sigma in
  Imp.random_gaussian state ~n ~dst ~mu ~sigma;
  dst

(*------------------------------------*)

open Format
open Hopp

let print fmt v =
  let n = size v in
  pp_open_lbox fmt "[|";
  for i = 0 to n-1 do
    if i > 0 then begin
      pp_print_string fmt ";";
      pp_print_cut fmt ()
    end;
    fprintf fmt "%12.6g" v.(i)
  done;
  pp_close_lbox fmt "|]"

let to_string v = pp_make_to_string print v

(*------------------------------------*)

let ( ~- ) = neg
let ( +  ) = add
let ( -  ) = sub
let ( *  ) = mul
let ( *. )  = smul
let ( /  ) = div
let ( /. ) = divs

(*----------------------------------------------------------------------------*)

(* open OUnit *)
(* open Printf *)

(* let test_equal msg u v = *)
(*   assert_equal *)
(*     ~msg *)
(*     ~cmp:(is_equal Float.delta) *)
(*     ~printer:to_string *)
(*     u v *)

(* let test_vector d = *)
(*   "Dim N" >::: [ *)
(*   ] *)

(* let test_vector2 = *)
(*   "Dim 2" >::: [ *)
(*     "swizzle2'" >:: begin fun _ -> *)
(*       let v = random_unit N._2 in *)
(*       let u = *)
(* 	swizzle2 `y`x  *)
(* 	  (swizzle2 `y`x v) *)
(*       in *)
(*       test_equal *)
(* 	"swizzled^2 == id" *)
(* 	u v *)
(*     end *)
(*   ] *)

(* let test_vector3 = *)
(*   "Dim 3" >::: [ *)
(*     "swizzle3'" >:: begin fun _ -> *)
(*       let v = random_unit N._3 in *)
(*       let u = *)
(* 	swizzle3 `x`y`z *)
(* 	  (swizzle3 `z`x`y *)
(* 	     (swizzle3 `z`x`y *)
(* 		(swizzle3 `z`x`y v))) *)
(*       in *)
(*       test_equal *)
(* 	"swizzle^3 == id" *)
(* 	u v *)
(*     end *)
(*   ] *)

(* let test_vector4 = *)
(*   "Dim 4" >::: [ *)
(*     "swizzle4'" >:: begin fun _ -> *)
(*       let v = random_unit N._4 in *)
(*       let u = *)
(* 	swizzle4 `x`y`z`w *)
(* 	  (swizzle4 `z`w`x`y  *)
(* 	     (swizzle4 `w`z`y`x  *)
(* 		(swizzle4 `y`x`w`z v))) *)
(*       in *)
(*       test_equal *)
(* 	"swizzled^4 == id" *)
(* 	u v *)
(*     end *)
(*   ] *)

(* let unit_test = *)
(*   "Vector" >::: [ *)
(*     test_vector N._1; *)
(*     test_vector N._2; *)
(*     test_vector N._3; *)
(*     test_vector N._4; *)
(*     test_vector2; *)
(*     test_vector3; *)
(*     test_vector4; *)
(*     "TODO" >:: fun _ -> todo "More assertions!"; *)
(*   ] *)

(*----------------------------------------------------------------------------*)
