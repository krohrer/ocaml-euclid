type ephemeral = Core.ephemeral
type persistent = Core.persistent

open Core
module V = Vector

type ('a,'n) t = {
  lower : ('a,'n) V.t;
  upper : ('a,'n) V.t
}

(*----------------------------------------------------------------------------*)

type cls = [`Out | `Hull | `In]

(*----------------------------------------------------------------------------*)

let copied i =
  let l = V.copied i.lower in
  let u = V.copied i.upper in
    { lower = l; upper = u }

let copy' i dst =
  V.copy' i.lower dst.lower;
  V.copy' i.lower dst.lower

(*------------------------------------*)
    
let get_lower' i dst =
  V.copy' i.lower dst
  
let get_upper' i dst =
  V.copy' i.upper dst

let get_delta' i dst =
  V.sub' i.upper i.lower dst

let lower i =
  i.lower

let upper i =
  i.upper

let delta i =
  V.sub i.upper i.lower
  
let set_lower' i v =
  V.copy' v i.lower

let set_upper' i v =
  V.copy' v i.upper

(*------------------------------------*)

let nothing' i =
  V.fill'     infinity i.lower;
  V.fill' neg_infinity i.upper

let everything' i =
  V.fill' neg_infinity i.lower;
  V.fill'     infinity i.upper

let nothing d = {
  lower = V.make d ~init:infinity;
  upper = V.make d ~init:neg_infinity
}

let everything d = {
  lower = V.make d ~init:neg_infinity;
  upper = V.make d ~init:infinity
}

let is_nothing i =
  V.is_uniform i.lower infinity &&
  V.is_uniform i.upper neg_infinity

let is_everything i =
  V.is_uniform i.lower neg_infinity &&
  V.is_uniform i.upper infinity

(*------------------------------------*)

let __absolve__ (i : (ephemeral,'n) t) : (_,'n) t = Obj.magic i
let __magic__ (i : (_,'n) t) : (_,'n) t = Obj.magic i

(*------------------------------------*)

let union' i k dst =
  V.min' i.lower k.lower dst.lower;
  V.max' i.upper k.upper dst.upper

let union i k = {
  lower = V.min i.lower k.lower;
  upper = V.max i.upper k.upper
}

let intersection' i k dst =
  V.max' i.lower k.lower dst.lower;
  V.min' i.upper k.upper dst.upper

let intersection i k = {
  lower = V.max i.lower k.lower;
  upper = V.max i.upper k.upper
}

let embrace' i v =
  V.min' v i.lower i.lower;
  V.max' v i.upper i.upper

let embrace i v = {
  lower = V.min v i.lower;
  upper = V.max v i.upper
}

let clamp' i v dst =
  V.max' i.lower v dst;
  V.min' dst i.upper dst

let clamp i v =
  let v = V.min i.upper v in
    V.max' v i.lower v;
    V.__absolve__ v

 (*------------------------------------*)

let is_inside1 b v i =
  (* is_nothing b --> is_inside b _ _ == false *)
  let l = V.get b.lower i and
      u = V.get b.upper i and
      x = V.get v i in
  l <= x && x <= u

let is_inside b v =
  let loop i n =
    if i < n then
    else
  in
  loop 0 (V.dim v)

let classify_value ~eps b v =

let disjoint ~eps i k =
  

let intersects ~eps i k =
  assert false

let contains ~eps i k =
  let lowercls = classify_value ~eps i k.lower and
      uppercls = classify_value ~eps i k.upper in
  match lowercls, uppercls with
  | `Outside, _
  | _, `Outside ->
    false
  | _, _ -> true

let is_equal ~eps i k =
  V.is_equal ~eps i.lower k.lower &&
  V.is_equal ~eps i.upper k.upper

open Format
open Hopp

let print fmt i =
  hopp_open_box fmt "{";
  hopp_labeled "lower" V.print fmt i.lower;
  pp_print_space fmt ();
  hopp_labeled "upper" V.print fmt i.upper;
  hopp_close_box fmt "}"

let to_string i = hopp_to_string print i

(*----------------------------------------------------------------------------*)

open OUnit

let test_equal msg a b =
  (** Needs cast because of assert_equal*)
  assert_equal
    ~msg
    ~cmp:(is_equal ~eps:Flt.delta)
    ~printer:to_string
    (__magic__ a) b

let unit_test =
  "Interval" >::: [
  ]

(*----------------------------------------------------------------------------*)
