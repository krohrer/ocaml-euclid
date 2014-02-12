(** {6 Unsafe mutable operations} *)
(*----------------------------------------------------------------------------*)

type t = float array

val alloc : n:int -> t

val init  : n:int -> dst:t -> (int -> float) -> unit
val fill  : n:int -> dst:t -> float -> unit
val unity : n:int -> dst:t -> int -> unit

val neg  : n:int -> dst:t -> t -> unit
val add  : n:int -> dst:t -> t -> t -> unit
val sub  : n:int -> dst:t -> t -> t -> unit
val mul  : n:int -> dst:t -> t -> t -> unit
val smul : n:int -> dst:t -> float -> t -> unit
val div  : n:int -> dst:t -> t -> t -> unit
val divs : n:int -> dst:t -> t -> float -> unit
val abs  : n:int -> dst:t -> t -> unit
val min  : n:int -> dst:t -> t -> t -> unit
val max  : n:int -> dst:t -> t -> t -> unit

val dot : n:int -> t -> t -> float
val len : n:int -> t -> float
val lensq : n:int -> t -> float

val cross : dst:t -> t -> t -> unit

val normalize : n:int -> dst:t -> t -> unit

val mid : n:int -> dst:t -> t -> t -> unit
val lerp : n:int -> t:float -> dst:t -> t -> t -> unit

val minmax : n:int -> dmin:t -> dmax:t -> t -> t -> unit

val swizzle2 : int -> int -> dst:t -> t -> unit
val swizzle3 : int -> int -> int -> dst:t -> t -> unit
val swizzle4 : int -> int -> int -> int -> dst:t -> t -> unit

val mask2 : bool -> bool -> dst:t -> t -> t -> unit
val mask3 : bool -> bool -> bool -> dst:t -> t -> t -> unit
val mask4 : bool -> bool -> bool -> bool -> dst:t -> t -> t -> unit

val random : Rnd.t -> n:int -> dst:t -> lower:t -> upper:t -> unit
val random_unit : Rnd.t -> n:int -> dst:t -> unit
val random_gaussian : Rnd.t -> n:int -> dst:t -> mu:t -> sigma:t -> unit

val is_equal : Float.eps -> n:int -> t -> t -> bool
val is_zero : Float.eps -> n:int -> t -> bool
