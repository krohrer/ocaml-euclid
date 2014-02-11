(** Constants *)
(*----------------------------------------------------------------------------*)

type t = float
type eps = private float

val make_eps	: float -> eps

val epsilon	: eps
val delta	: eps

(** Operators / shorthands *)
(*----------------------------------------------------------------------------*)

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

val min : float -> float -> float
val max : float -> float -> float

(** Comparison *)
(*----------------------------------------------------------------------------*)

val compare_with_zero	: eps -> float ->          int
val compare		: eps -> float -> float -> int

val is_equal	: eps -> float -> float -> bool
val is_lt	: eps -> float -> float -> bool
val is_gt	: eps -> float -> float -> bool

val is_zero	: eps -> float -> bool
val is_negative : eps -> float -> bool
val is_positive : eps -> float -> bool

(** Conversion *)
(*----------------------------------------------------------------------------*)

external to_bits : float -> int64 = "caml_int64_bits_of_float"
external of_bits : int64 -> float = "caml_int64_float_of_bits"

(** Unit testing  *)
(*----------------------------------------------------------------------------*)

val test_equal : string -> float -> float -> unit
