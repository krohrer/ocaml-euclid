(*----------------------------------------------------------------------------*)

let epsilon = epsilon_float
let delta = sqrt epsilon

let compare_with_zero ~eps d =
  if d < -.eps then
    `L
  else if d > eps then
    `G
  else
    `EQ

let compare ~eps a b =
  compare_with_zero ~eps (a -. b)

let is_equal ~eps a b =
  compare ~eps a b = `EQ 

let is_lt ~eps a b =
  compare ~eps a b = `L

let is_gt ~eps a b =
  compare ~eps a b = `G

let is_zero ~eps a =
  compare_with_zero ~eps a = `EQ

let is_negative ~eps a =
  compare_with_zero ~eps a = `L

let is_positive ~eps a =
  compare_with_zero ~eps a = `G

(*------------------------------------*)

open OUnit

let test_equal msg u v =
  assert_equal
    ~msg
    ~cmp:(is_equal ~eps:delta)
    ~printer:string_of_float
    u v

(*----------------------------------------------------------------------------*)
