(** Random number generator *)

type t

val default : t

val make : ?seed:int array -> unit -> t
val copy : t -> t

val bits : t -> int
val int : t -> int -> int
val int32 : t -> Int32.t -> Int32.t
val nativeint : t -> Nativeint.t -> Nativeint.t
val int64 : t -> Int64.t -> Int64.t
val float : t -> float -> float
val bool : t -> bool

val gaussian : t -> ?mu:float -> ?sigma:float -> unit -> float
