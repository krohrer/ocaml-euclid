(** Typed natural numbers (From 0 to 10)
    
    Using peano type arithmetic
*)

type zero
type 'a t = private int
type 'a s

type 'a s0 = 'a
type 'a s1 = 'a s
type 'a s2 = 'a s s
type 'a s3 = 'a s s s
type 'a s4 = 'a s s s s
type 'a s5 = 'a s s s s s
type 'a s6 = 'a s s s s s s
type 'a s7 = 'a s s s s s s s
type 'a s8 = 'a s s s s s s s s
type 'a s9 = 'a s s s s s s s s s
type 'a s10 = 'a s s s s s s s s s s

type _0 = zero s0
type _1 = zero s1
type _2 = zero s2
type _3 = zero s3
type _4 = zero s4
type _5 = zero s5
type _6 = zero s6
type _7 = zero s7
type _8 = zero s8
type _9 = zero s9
type _10 = zero s10

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
val i     : 'a t -> ('a s t -> 'b) -> 'b
val num   : 'a t -> 'a t

val int : 'a t -> int
val __num_of_int__ : int -> 'a t

val pred : 'a s t -> 'a t
val s : 'a t -> 'a s t

(*------------------------------------*)

val print : Format.formatter -> _ t -> unit

(*----------------------------------------------------------------------------*)
 
