(** Random numbers *)

type state

val default_state : state

val make_state : unit -> state
val make_state_with_seed : int array -> state

val int : state -> int -> int
val float : state -> float -> float
val bool : state -> bool

val gaussian : state -> ?mu:float -> ?sigma:float -> unit -> float
