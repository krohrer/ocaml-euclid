(** Angular measurement *)

type t

(** {6 Creation} *)
(*----------------------------------------------------------------------------*)

val radian : float -> t
val to_radian : t -> float

val degree : float -> t
val to_degree : t -> float

val zero : t

val half_circle : t
val full_circle : t

val random : ?state:Rnd.state -> ?range:t -> unit -> t

(** {6 Conversion} *)
(*----------------------------------------------------------------------------*)

val degree_of_radian : float -> float
val radian_of_degree : float -> float

val neg : t -> t
val sub : t -> t -> t
val add : t -> t -> t
val mul : t -> float -> t
val div : t -> float -> t

val cos : t -> float
val sin : t -> float
val tan : t -> float

val acos : float -> t
val asin : float -> t
val atan : float -> t

val normalize : t -> t

(*------------------------------------*)

val is_equal : eps:float -> t -> t -> bool

(** {6 Printing} *)
(*----------------------------------------------------------------------------*)

val to_string : t -> string
val print : Format.formatter -> t -> unit

(** {6 Unit testing} *)
(*----------------------------------------------------------------------------*)

val test_equal : string -> t -> t -> unit
val unit_test : OUnit.test
