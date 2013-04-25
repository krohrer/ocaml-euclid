open Core

type t = float

let pi_h = acos 0.
let degree_of_radian x = x *. 90. /. pi_h
let radian_of_degree x = x *. pi_h /. 90.

let degree d:float = d
let to_degree a:t = a

let radian r = degree_of_radian r
let to_radian a = radian_of_degree a

let zero = 0.

let half_circle = degree 180.
let full_circle = degree 360.

let neg = ( ~-. )
let sub = ( -. )
let add = ( +. )
let mul = ( *. )
let div = ( /. )

let sin a = sin (to_radian a)
let cos a = cos (to_radian a)
let tan a = tan (to_radian a)

let asin a = radian (asin a)
let acos a = radian (acos a)
let atan a = radian (atan a)

let normalize a =
  let d = to_degree a in
  let n = mod_float d 360. in
    degree (if n >= 0. then n else n +. 360.)
 
(*------------------------------------*)

open Format
open Hopp

let print fmt angle =
  fprintf fmt "%f degree" (to_degree angle)

let to_string = pp_make_to_string print

(*------------------------------------*)

let is_equal = Flt.is_equal

let random ?(state=Rnd.default_state) ?(range=full_circle) () =
  Rnd.float state range

(*----------------------------------------------------------------------------*)

open OUnit

let test_equal msg a b  =
  assert_equal
    ~msg
    ~cmp:(is_equal ~eps:Flt.delta)
    ~printer:to_string
    a b

let unit_test =
  "Angle" >::: [
    "random, degree, degree_of_radian, to_radian" >:: begin fun _ ->
      let a = random () in
	test_equal "degree (degree_of_radian (to_radian a)) == a"
	  (degree (degree_of_radian (to_radian a)))
	  a
    end;
    "random, radian, radian_of_degree, to_degree" >:: begin fun _ ->
      let a = random () in
	test_equal "degree (to_degree a) == a" a (degree (to_degree a))
    end;
    "random, mul, div" >:: begin fun _ ->
      let a = random () in
	test_equal "mul (div a 2.) 2. == a" a (mul (div a 2.) 2.)
    end;
    "random, add, sub, neg" >:: begin fun _ ->
      let a = random () in
	test_equal "add a (neg a) = sub a a" 
	  (add a (neg a))
	  (sub a a)
    end;
    "normalize, full_circle, zero" >:: begin fun _ ->
      test_equal "normalize full_circle == zero"
	(normalize full_circle)
	zero
    end;
  ]

(*----------------------------------------------------------------------------*)
