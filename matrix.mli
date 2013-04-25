(* Kaspar Rohrer, Mon Jan 19 23:31:11 CET 2009 *)

(** Matrix (persistent/ephemeral) of floats of statically typed dimensions *)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

type ('a,'m,'n) t

(** {6 Helper types} *)
(*----------------------------------------------------------------------------*)

type majority = [`row_major | `column_major]

(** {6 Creation / Initialization} *)
(*----------------------------------------------------------------------------*)

val make :  ?init:float -> 'm N.t -> 'n  N.t -> (_,'m ,'n ) t

val zero : 'm  N.t -> 'n  N.t -> (_,'m ,'n ) t
val identity : 'n  N.t -> (_,'n ,'n ) t

val fill' : float -> (ephemeral,_,_) t -> unit
val zero' : (ephemeral,_,_) t -> unit
val identity' : (ephemeral,'n,'n) t -> unit

val random : ?state:Rnd.state -> 'm  N.t -> 'n  N.t -> (_,'m,'n) t
val random_invertible : ?state:Rnd.state -> 'n N.t -> (_,'m,'n) t

(** {6 Conversion and copying} *)
(*----------------------------------------------------------------------------*)

val copied : (_,'m,'n) t -> (_,'m,'n) t

val copy' : (_,'m,'n) t -> (ephemeral,'m,'n) t -> unit

(** Copies the common fields between matrices of different size *)
val xfer' : (_,_,_) t -> (ephemeral,_,_) t -> unit

(**  *)
val of_array : majority -> 'm  N.t -> 'n  N.t -> float array -> (ephemeral,'m ,'n ) t
val to_array : majority -> (ephemeral,'m,'n) t -> float array

(** {6 Representation} *)
(*----------------------------------------------------------------------------*)

val of_arrays : 'm N.t -> 'n N.t -> float array array -> (_,'m ,'n ) t
val to_arrays : (_,'m,'n) t -> float array array

(** Escape from local mutability. *)
val __absolve__ : (ephemeral,'m,'n) t -> (_,'m,'n) t
(** Phantom-type magic *)
val __magic__ : (_,'m,'n) t -> (_,'m,'n) t

(** {6 Access} *)
(*----------------------------------------------------------------------------*)

val rows : (_,'m,_) t -> int
val columns : (_,_,'n) t -> int
val row_dimension : (_,'m,_) t -> 'm N.t
val column_dimension : (_,_,'n) t -> 'n N.t

val get : (_,_,_) t -> int -> int -> float
val set' : (ephemeral,_,_) t -> int -> int -> float -> unit

val row : ('a,_,'n) t -> int -> ('a,'n) Vector.t
val column : (persistent,'m,_) t -> int -> (persistent,'m) Vector.t
val diagonal : (persistent,'n,'n) t -> (persistent,'n) Vector.t

val get_minor' : (_,'m N.succ, 'n N.succ) t -> int -> int -> (ephemeral,'m,'n) t -> unit

val get_row' : (_,_,'n) t -> int -> (ephemeral,'n) Vector.t -> unit
val get_column' : (_,'m,_) t -> int -> (ephemeral,'m) Vector.t -> unit
val get_diagonal' : (_,'n,'n) t -> (ephemeral,'n) Vector.t -> unit
val set_row' : (ephemeral,_,'n) t -> int -> (_,'n) Vector.t -> unit
val set_column' : (ephemeral,'m,_) t -> int -> (_,'m) Vector.t -> unit
val set_diagonal' : (ephemeral,'n,'n) t -> (_,'n) Vector.t -> unit

(** {6 Ephemeral operations} *)
(*----------------------------------------------------------------------------*)

val neg'  : (_,'m,'n) t -> (ephemeral,'m,'n) t -> unit
val sub'  : (_,'m,'n) t -> (_,'m,'n) t -> (ephemeral,'m,'n) t -> unit
val add'  : (_,'m,'n) t -> (_,'m,'n) t -> (ephemeral,'m,'n) t -> unit
val mul1' : (_,'m,'n) t -> float -> (ephemeral,'m,'n) t -> unit
val mulm' : (_,'m,'n) t -> (_,'n,'s) t -> (ephemeral,'m,'s) t -> unit
val mulv' : (_,'m,'n) t -> (_,'n) Vector.t -> (ephemeral,'m) Vector.t -> unit
val vmul' : (_,'m) Vector.t -> (_,'m,'n) t -> (ephemeral,'n) Vector.t -> unit

val invert'   : (_,'n,'n) t -> (ephemeral,'n,'n) t -> unit
val transpose' : (_,'n,'m) t -> (ephemeral,'m,'n) t -> unit

(** {6 Pure operations} *)
(*----------------------------------------------------------------------------*)

val neg  : (_,'m,'n) t -> (_,'m,'n) t
val sub  : (_,'m,'n) t -> (_,'m,'n) t -> (_,'m,'n) t
val add  : (_,'m,'n) t -> (_,'m,'n) t -> (_,'m,'n) t
val mul1 : (_,'m,'n) t -> float -> (_,'m,'n) t
val mulm : (_,'l,'m) t -> (_,'m,'n) t -> (_,'l,'n) t
val mulv : (_,'m,'n) t -> (_,'n) Vector.t -> (_,'m) Vector.t
val vmul : (_,'m) Vector.t -> (_,'m,'n) t -> (_,'n) Vector.t

val inverted : (_,'n,'n) t -> (_,'n,'n) t
val transposed : (_,'m,'n) t -> (_,'n,'m) t

val det : (_,'n,'n) t -> float

(** {6 Predicates} *)
(*----------------------------------------------------------------------------*)

val is_zero : eps:float -> (_,'n,'n) t -> bool
val is_equal : eps:float -> (_,'m,'n) t -> (_,'m,'n) t -> bool
val is_identity : eps:float -> (_,'m,'n) t -> bool

(** {6 Printing} *)
(*----------------------------------------------------------------------------*)

val to_string : (_,_,_) t -> string
val print : Format.formatter -> (_,_,_) t -> unit

(** {6 Unit testing} *)
(*----------------------------------------------------------------------------*)

val test_equal : string -> (_,'m,'n) t -> (_,'m,'n) t -> unit
val unit_test : OUnit.test



