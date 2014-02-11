(*----------------------------------------------------------------------------*)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

open Core

type 'a t = {
  mutable lower : float;
  mutable upper : float
}

(*----------------------------------------------------------------------------*)

type cls = [`Out | `Eps | `In]

(*----------------------------------------------------------------------------*)

let make v0 v1 =
  if v0 > v1 then {
    lower = v1;
    upper = v0
  } else {
    lower = v0;
    upper = v1
  }

let make_point v = {
  lower = v;
  upper = v
}

let copied i = {
  lower = i.lower;
  upper = i.upper
}

let copy' i dst =
  dst.lower <- i.lower;
  dst.upper <- i.upper

(*------------------------------------*)
    
let lower i =
  i.lower

let upper i =
  i.upper

let delta i =
  i.upper -. i.lower

(*------------------------------------*)

let nothing' dst =
  dst.lower <- infinity;
  dst.upper <- neg_infinity

let everything' dst =
  dst.lower <- neg_infinity;
  dst.upper <- infinity

let nothing = {
  lower = infinity;
  upper = neg_infinity
}

let everything = {
  lower = neg_infinity;
  upper = infinity
}

let is_nothing i =
  i.lower > i.upper

let is_everything i =
  i.lower = neg_infinity &&
  i.upper = infinity

let random_gaussian ?(state=Rnd.default_state) ?(mu=0.) ?(sigma=1.) () =
  let x0 = Rnd.gaussian state ~mu ~sigma () in
  let x1 = Rnd.gaussian state ~mu ~sigma () in
    if x0 < x1 then {
      lower = x0;
      upper = x1
    } else {
      lower = x1;
      upper = x0
    }

(*------------------------------------*)

let __absolve__ i = (i : ephemeral t:> _ t)
let __magic__ i = (i : _ t :> _ t)

(*------------------------------------*)

let union' i k dst =
  if is_nothing i then
    copy' k dst
  else if is_nothing k then
    copy' i dst
  else begin
    dst.lower <- min i.lower k.lower;
    dst.upper <- min i.upper k.upper
  end

let union i k =
  if is_nothing i then
    k
  else if is_nothing k then
    i
  else {
    lower = min i.lower k.lower;
    upper = max i.upper k.upper
  }

let intersection' i k dst =
  if is_nothing i || is_nothing k then
    copy' nothing dst
  else begin
    dst.lower <- max i.lower k.lower;
    dst.upper <- min i.upper k.upper
  end

let intersection i k =
  if is_nothing i || is_nothing k then
    nothing
  else {
    lower = max i.lower k.lower;
    upper = min i.upper k.upper
  }

let embrace_value' i v dst =
  dst.lower <- min v i.lower;
  dst.upper <- max v i.upper

let embrace_value i v = {
  lower = min v i.lower;
  upper = max v i.upper
}

let grow' i delta dst =
  let l = i.lower -. delta and
      u = i.upper +. delta
  in
    if l > u then
      copy' nothing dst
    else begin
      dst.lower <- l;
      dst.upper <- u
    end

let grow i delta =
  let l = i.lower -. delta and
      u = i.upper +. delta
  in
    if l > u then
      nothing
    else {
      lower = l;
      upper = u
    }

let clamp_value i v =
  if is_nothing i then Printf.eprintf "undefined: clamp_value nothing %g" v;
  min i.upper (max i.lower v)

(*------------------------------------*)

let intersects i k =
  not (is_nothing i) && not (is_nothing k) && (k.upper >= i.lower && i.upper >= k.lower)

let contains i k =
  (intersection i k) = k

let contains_value i v =
  i.lower <= v && v <= i.upper

let classify_value ~eps i v =
  if is_nothing i then
    `Nothing
  else
    match Scalar.compare ~eps i.lower v with
      |  `L -> `Below
      | `EQ -> `Lower
      |  `G ->
	   match Scalar.compare ~eps i.upper v with
	     |  `L -> `Inside
	     | `EQ -> `Upper
	     |  `G -> `Above

let binary_search_strictly_increasing ?(n=23) ?(eps=Scalar.epsilon) ~f range y =
  let rec iter i x0 x1 =
    let xm = 0.5 *. (x0 +. x1) in    if i > 0 then
	if x0 = xm && xm = x1 then
	  xm
	else
	  match Scalar.compare ~eps (f xm) y with
	    | `L -> iter (pred i) x0 xm
	    | `EQ -> xm
	    | `G -> iter (pred i) xm x1
      else
	xm
  in
    assert (is_nothing range = false);
    let x0 = range.lower in
    let x1 = range.upper in
      iter n x0 x1
	
let is_equal ~eps i k =
  (is_nothing i &&
     is_nothing k) ||
    (Scalar.is_equal ~eps i.lower k.lower &&
       Scalar.is_equal ~eps i.upper k.upper)

(*----------------------------------------------------------------------------*)

open Format
open Hopp

let print fmt i =
  if is_nothing i then
    pp_print_string fmt "Nothing"
  else if is_everything i then
    pp_print_string fmt "Everything"
  else begin
    pp_open_lbox fmt "{";
    pp_print_labeled_field "lower" pp_print_float fmt i.lower;
    pp_print_separator fmt ";";
    pp_print_labeled_field "upper" pp_print_float fmt i.upper;
    pp_close_lbox fmt "}"
  end

let to_string i = pp_make_to_string print i

(*----------------------------------------------------------------------------*)

open OUnit
open Printf

let test_equal msg a b =
  (** Needs cast because of assert_equal*)
  assert_equal
    ~msg
    ~cmp:(is_equal ~eps:Scalar.delta)
    ~printer:to_string
    (__magic__ a) b

let test_binop f (msg,a,b,r) =
  assert_equal ~msg ~cmp:(is_equal ~eps:Scalar.delta) ~printer:to_string (f a b) r

let test_binpred p (msg,a,b,r) =
  assert_bool msg ((p a b) == r)

let test_pred p (msg,a,r) =
  assert_bool msg ((p a) == r)

let classify_value_suite = "classify_value" >:: fun () ->
  let r = Rnd.gaussian Rnd.default_state () in
    List.iter (fun (msg,a,v,r) -> assert_equal ~msg (classify_value ~eps:1.0 a v) r) [
      "cls 0 r=Out",nothing, r, `Out;
      "cls r r=Eps",make_point r, r, `Eps;
      "cls 1 r=In",everything, r, `In;
    ]

let intersection_suite = "intersection" >:: fun () ->
  let r = random_gaussian () in
    List.iter (test_binop intersection) [
      "0*0=0",nothing,nothing,nothing;
      "0*r=0",nothing,r,nothing;
      "0*1=0",nothing,everything,nothing;
      "r*0=0",r,nothing,nothing;
      "r*r=r",r,r,r;
      "r*1=r",r,everything,r;
      "1*0=0",everything,nothing,nothing;
      "1*r=r",everything,r,r;
      "1*1=1",everything,everything,everything
    ]

let intersects_suite = "intersects" >:: fun () ->
  let r = random_gaussian () in
    List.iter (test_binpred intersects) [
      "0*0=0",nothing,nothing,false;
      "0*r=0",nothing,r,false;
      "0*1=0",nothing,everything,false;
      "r*0=0",r,nothing,false;
      "r*r=r",r,r,true;
      "r*1=r",r,everything,true;
      "1*0=0",everything,nothing,false;
      "1*r=r",everything,r,true;
      "1*1=1",everything,everything,true
    ]

let union_suite = "union" >:: fun () ->
  let r = random_gaussian () in
    List.iter (test_binop union) [
      "0+0=0",nothing,nothing,nothing;
      "0+r=r",nothing,r,r;
      "0+1=1",nothing,everything,everything;
      "r+0=r",r,nothing,r;
      "r+r=r",r,r,r;
      "r+1=1",r,everything,everything;
      "1+0=1",everything,nothing,everything;
      "1+r=1",everything,r,everything;
      "1+r=1",everything,everything,everything
    ]

let contains_suite = "contains" >:: fun () ->
  let r = random_gaussian () in
    List.iter (test_binpred contains) [
      "0>=0=T",nothing,nothing,true;
      "0>=r=F",nothing,r,false;
      "0>=1=F",nothing,everything,false;
      "r>=0=T",r,nothing,true;
      "r>=r=T",r,r,true;
      "r>=1=F",r,everything,false;
      "1>=0=T",everything,nothing,true;
      "1>=r=T",everything,r,true;
      "1>=1=T",everything,everything,true;
    ]

let contains_value_suite = "contains_value" >:: fun () ->
  let r = Rnd.gaussian Rnd.default_state () in
    List.iter (test_binpred contains_value) [
      "r in 0", nothing, r, false;
      "r in r", make_point r, r, true;
      "r in 1", everything, r, true
    ]

let grow_suite = "grow" >:: fun () ->
  let r = random_gaussian () in
    assert_bool "contains(grow(r,1),r)" (contains (grow r 1.) r);
    assert_bool "not contains(grow(r,-1),r)" (not (contains (grow r (-1.)) r));
    assert_bool "grow(r,-delta(r)-eps)=0" (is_nothing (grow r (-.(0.1 +. delta r))))

let unit_test =
  "Interval" >::: [
    intersection_suite;
    intersects_suite;
    union_suite;
    contains_suite;
    contains_value_suite;
    grow_suite
  ]

(*----------------------------------------------------------------------------*)
