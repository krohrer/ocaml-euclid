(* Kaspar Rohrer, Tue Mar  2 23:28:16 CET 2010 *)

(*----------------------------------------------------------------------------*)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

type ('a,'d) t

val of_vector : ('a,'d) Vector.t -> ('a,'d) t
val to_vector : ('a,'d) Vector.t -> ('a,'d) t

val delta : (_,'d) t -> (_,'d) t -> (_,'d) Vector.t
val distance : (_,'d) t -> (_,'d) t -> float
val distance_squared : (_,'d) t -> (_,'d) t -> float

val print : Format.formatter -> (_,_) t -> unit

(*----------------------------------------------------------------------------*)
