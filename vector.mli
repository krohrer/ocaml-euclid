(** Vector of floats of statically typed dimension *)

type 'n t

(** {6 Helper types} *)
(*----------------------------------------------------------------------------*)

type c1 = [`x]
type c2 = [`x|`y]
type c3 = [`x|`y|`z]
type c4 = [`x|`y|`z|`w]

(** {6 Creation / Initialization} *)
(*----------------------------------------------------------------------------*)

val init : 'n N.t -> (int -> float) -> 'n t
val fill : 'n N.t -> float -> 'n t

val make1 : float -> N._1 t
val make2 : float -> float -> N._2 t
val make3 : float -> float -> float -> N._3 t
val make4 : float -> float -> float -> float -> N._4 t

val zero : 'n N.t -> 'n t
val one  : 'n N.t -> 'n t

(* Unity vectors *)
val ex : (_ N.s1 as 'n) N.t -> 'n t
val ey : (_ N.s2 as 'n) N.t -> 'n t
val ez : (_ N.s3 as 'n) N.t -> 'n t
val ew : (_ N.s4 as 'n) N.t -> 'n t

(** {6 Randomization} *)
(*----------------------------------------------------------------------------*)

val random : ?state:Rnd.t -> ?lower:'n t -> ?upper:'n t -> 'n N.t -> 'n t
val random_unit : ?state:Rnd.t -> 'n N.t -> 'n t
val random_gaussian : ?state:Rnd.t -> ?mu:'n t -> ?sigma:'n t -> 'n N.t -> 'n t

(** {6 Conversion and copying} *)
(*----------------------------------------------------------------------------*)

val to_array : _ t -> float array
val from_array : float array -> 'n N.t -> 'n t

(** {6 Representation} *)
(*----------------------------------------------------------------------------*)

external __repr__ : 'n t -> ImpVec.t = "%identity"

(** {6 Access} *)
(*----------------------------------------------------------------------------*)

val size : _ t -> int
val dimension : _ t -> 'n N.t

val get : int -> _ t -> float

val x : _ N.s1 t -> float
val y : _ N.s2 t -> float
val z : _ N.s3 t -> float
val w : _ N.s4 t -> float

(** {6 Operations} *)
(*----------------------------------------------------------------------------*)

val ( ~- ) : 'n t -> 'n t
val ( +  ) : 'n t -> 'n t -> 'n t
val ( -  ) : 'n t -> 'n t -> 'n t
val ( *  ) : 'n t -> 'n t -> 'n t
val ( *. ) : float-> 'n t -> 'n t
val ( /  ) : 'n t -> 'n t -> 'n t
val ( /. ) : 'n t -> float-> 'n t
    
val neg  : 'n t -> 'n t
val add  : 'n t -> 'n t -> 'n t
val sub  : 'n t -> 'n t -> 'n t
val mul  : 'n t -> 'n t -> 'n t
val smul : float-> 'n t -> 'n t
val div  : 'n t -> 'n t -> 'n t
val divs : 'n t -> float-> 'n t

val abs  : 'n t -> 'n t
val min  : 'n t -> 'n t -> 'n t
val max  : 'n t -> 'n t -> 'n t

val dot : 'n t -> 'n t -> float

val cross : N._3 t -> N._3 t -> N._3 t

val normalize	: 'n t -> 'n t

val mid		: 'n t -> 'n t -> 'n t
val lerp 	: t:float -> 'n t -> 'n t -> 'n t
val minmax 	: 'n t -> 'n t -> 'n t * 'n t

val swizzle2 : c2 -> c2 -> N._2 t -> N._2 t
val swizzle3 : c3 -> c3 -> c3 -> N._3 t -> N._3 t
val swizzle4 : c4 -> c4 -> c4 -> c4 -> N._3 t -> N._3 t

val len : 'n t -> float
val lensq : 'n t -> float

(** {6 Predicates} *)
(*----------------------------------------------------------------------------*)

val is_equal : Float.eps -> 'n t -> 'n t -> bool
val is_zero : Float.eps -> 'n t -> bool

(** {6 Printing} *)
(*----------------------------------------------------------------------------*)

val to_string : _ t -> string
val print : Format.formatter -> _ t -> unit

(** {6 Unit testing} *)
(*----------------------------------------------------------------------------*)

(* val test_equal : string -> 'n t -> 'n t -> unit *)
(* val unit_test : OUnit.test *)
