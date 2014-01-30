(** Constants *)
(*----------------------------------------------------------------------------*)

val epsilon : float
val delta : float

(** Comparison *)
(*----------------------------------------------------------------------------*)

val compare_with_zero : eps:float -> float -> [`L | `G | `EQ]
val compare : eps:float -> float -> float -> [`L | `G | `EQ]

val is_equal : eps:float -> float -> float -> bool
val is_lt : eps:float -> float -> float -> bool
val is_gt : eps:float -> float -> float -> bool

val is_zero : eps:float -> float -> bool
val is_negative : eps:float -> float -> bool
val is_positive : eps:float -> float -> bool

(** Unit testing  *)
(*----------------------------------------------------------------------------*)

val test_equal : string -> float -> float -> unit
