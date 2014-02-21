(** Strongly-typed angular measurement (either radians or degrees)

    This is a very light abstraction which should come with no
    overhead when compared with raw floats. *)
(*----------------------------------------------------------------------------*)

type rad
type deg
type 'a t

(** {6 Creation & Conversion} *)
(*----------------------------------------------------------------------------*)

external rad : float -> rad t = "%identity"
external deg : float -> deg t = "%identity"

external rad_as_float : rad t -> float = "%identity"
external deg_as_float : deg t -> float = "%identity"

val rad_to_deg : rad t -> deg t
val deg_to_rad : deg t -> rad t

val pi : rad t
val tau : rad t

val half_circle : deg t
val full_circle : deg t

val normalize : period:'a t -> 'a t -> 'a t

(** {6 Operations} *)
(*----------------------------------------------------------------------------*)

external ( ~- ) : 'a t -> 'a t = "%negfloat"
external ( +  ) : 'a t -> 'a t -> 'a t = "%identity"
external ( -  ) : 'a t -> 'a t -> 'a t = "%identity"
external ( *. ) : float-> 'a t -> 'a t = "%mulfloat"
external ( /. ) : 'a t -> float-> 'a t = "%divfloat"

external abs : 'a t -> 'a t = "%absfloat"

val min : 'a t -> 'a t -> 'a t
val max : 'a t -> 'a t -> 'a t

external acos : float -> rad t = "caml_acos_float" "acos" "float"
external asin : float -> rad t = "caml_asin_float" "asin" "float"
external atan : float -> rad t = "caml_atan_float" "atan" "float"
external atan2 : float -> float -> rad t = "caml_atan2_float" "atan2" "float"

external cos : rad t -> float = "caml_cos_float" "cos" "float"
external sin : rad t -> float = "caml_sin_float" "sin" "float"
external tan : rad t -> float = "caml_tan_float" "tan" "float"

external ( <  )  : 'a t -> 'a t -> bool = "%lessthan"
external ( >  )  : 'a t -> 'a t -> bool = "%greaterthan"
external ( <= )  : 'a t -> 'a t -> bool = "%lessequal"
external ( >= )  : 'a t -> 'a t -> bool = "%greaterequal"
external compare : 'a t -> 'a t -> int = "%compare"

(** {6 Printing} *)
(*----------------------------------------------------------------------------*)

val print : Format.formatter -> 'a t -> unit
