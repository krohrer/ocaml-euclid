(*--------------------------------------------------------------------------*)

type t = float
type eps = float

let make_eps e = e

let epsilon = epsilon_float
let delta = sqrt epsilon

(*--------------------------------------------------------------------------*)

external ( ~- ) : float -> float = "%negfloat"
external ( ~+ ) : float -> float = "%identity"
external ( +  ) : float -> float -> float = "%addfloat"
external ( -  ) : float -> float -> float = "%subfloat"
external ( *  ) : float -> float -> float = "%mulfloat"
external ( /  ) : float -> float -> float = "%divfloat"

external f' : int -> float = "%floatofint"
external i' : float -> int = "%intoffloat"
external abs : float -> float = "%absfloat"
external ( mod ) : float -> float -> float = "caml_fmod_float" "fmod" "float"

external to_bits : float -> int64 = "caml_int64_bits_of_float"
external of_bits : int64 -> float = "caml_int64_float_of_bits"

let min a b = if a < b then a else b
let max a b = if a < b then b else a

(*--------------------------------------------------------------------------*)

let cmp_with_zero eps d =
  if d < -.eps then
    -1
  else if eps < d then
    1
  else
    0

let cmp eps a b =
  a -. b

let cmp eps a b =
  cmp_with_zero eps (a -. b)

let is_equal eps a b =
  cmp eps a b = 0

let is_lt eps a b =
  cmp eps a b < 0

let is_gt eps a b =
  0 < cmp eps a b

let is_zero eps a =
  cmp_with_zero eps a = 0

let is_negative eps a =
  cmp_with_zero eps a < 0

let is_positive eps a =
  0 < cmp_with_zero eps a

(*------------------------------------*)

external ( <  )  : float -> float -> bool = "%lessthan"
external ( >  )  : float -> float -> bool = "%greaterthan"
external ( <= )  : float -> float -> bool = "%lessequal"
external ( >= )  : float -> float -> bool = "%greaterequal"
external compare : float -> float -> int = "%compare"

(*------------------------------------*)

open OUnit

let test_equal msg u v =
  assert_equal
    ~msg
    ~cmp:(is_equal delta)
    ~printer:string_of_float
    u v

(*----------------------------------------------------------------------------*)
