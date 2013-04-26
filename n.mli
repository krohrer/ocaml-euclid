(** Typed natural numbers (From 0 to 10)
    
    Using peano type arithmetic
*)

type zero
type 'a t
type 'a succ

type 'a succ0 = 'a
type 'a succ1 = 'a succ
type 'a succ2 = 'a succ succ
type 'a succ3 = 'a succ succ succ
type 'a succ4 = 'a succ succ succ succ
type 'a succ5 = 'a succ succ succ succ succ
type 'a succ6 = 'a succ succ succ succ succ succ
type 'a succ7 = 'a succ succ succ succ succ succ succ
type 'a succ8 = 'a succ succ succ succ succ succ succ succ
type 'a succ9 = 'a succ succ succ succ succ succ succ succ succ
type 'a succ10 = 'a succ succ succ succ succ succ succ succ succ succ

type _0 = zero succ0
type _1 = zero succ1
type _2 = zero succ2
type _3 = zero succ3
type _4 = zero succ4
type _5 = zero succ5
type _6 = zero succ6
type _7 = zero succ7
type _8 = zero succ8
type _9 = zero succ9
type _10 = zero succ10

val _0 : _0 t
val _1 : _1 t
val _2 : _2 t
val _3 : _3 t
val _4 : _4 t
val _5 : _5 t
val _6 : _6 t
val _7 : _7 t
val _8 : _8 t
val _9 : _9 t
val _10 : _10 t

val unary : (zero t -> 'a) -> 'a
val i     : 'a t -> ('a succ t -> 'b) -> 'b
val num   : 'a t -> 'a t

val int : 'a t -> int
val __num_of_int__ : int -> 'a t

val pred : 'a succ t -> 'a t
val succ : 'a t -> 'a succ t

(*------------------------------------*)

val print : Format.formatter -> _ t -> unit

(*----------------------------------------------------------------------------*)
