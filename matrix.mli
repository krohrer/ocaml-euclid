(** Matrix of floats of statically typed dimensions MxN (rows, cols)

    Square matrices are MxM.
*)

type ('m,'n) t
type 'n dim = 'n N.t
type 'n vec = 'n Vector.t

(** {6 Helper types} *)
(*----------------------------------------------------------------------------*)

exception NotInvertible

(** {6 Creation / Initialization} *)
(*----------------------------------------------------------------------------*)

val init : 'm N.t -> 'n N.t -> (int -> int -> float) -> ('m,'n) t

val make_cols : 'm vec array -> 'n dim -> ('m,'n) t
val make_rows : 'm dim -> 'n vec array -> ('m,'n) t

val zero : 'm dim -> ('m,'m) t
val identity  : 'm dim -> ('m,'m) t

(* {6 Randomization} *)
(*----------------------------------------------------------------------------*)

val random : Rnd.t -> 'm dim -> 'n dim -> ('m,'n) t
val random_invertible : Rnd.t -> 'm N.t -> ('m,'m) t

(** {6 Conversion and copying} *)
(*----------------------------------------------------------------------------*)

val to_rows : ('m,'n) t -> ('n vec) array
val to_cols : ('m,'n) t -> ('m vec) array

val to_array : [`column_major|`row_major] -> ('m,'n) t -> float array
val from_array : [`column_major|`row_major] -> 'm dim -> 'n dim -> float array -> ('m,'n) t

(** {6 Representation} *)
(*----------------------------------------------------------------------------*)

val __make__ : 'm dim -> 'n dim -> ImpMat.t -> ('m,'n) t
val __repr__ : ('m,'n) t -> ImpMat.t

(** {6 Access} *)
(*----------------------------------------------------------------------------*)

type 'a idx (** Type-safe index *)

val x : 'a N.s1 idx
val y : 'a N.s2 idx
val z : 'a N.s3 idx
val w : 'a N.s4 idx

val rows : ('m,_) t -> int
val cols : (_,'n) t -> int
val row_dim : ('m,_) t -> 'm N.t
val col_dim : (_,'n) t -> 'n N.t

val get : ('m,'n) t -> 'm idx -> 'n idx -> float

val row : (_,'n) t -> 'n idx -> 'n vec
val col : ('m,_) t -> 'm idx -> 'm vec
val diagonal : ('n,'n) t -> 'n vec

val get' : (_,_) t -> int -> int -> float
val row' : (_,'n) t -> int -> 'n vec
val col' : ('m,_) t -> int -> 'm vec

(** {6 Operations} *)
(*----------------------------------------------------------------------------*)

val ( ~- ) : ('m,'n) t -> ('m,'n) t
val ( +  ) : ('m,'n) t -> ('m,'n) t -> ('m,'n) t
val ( -  ) : ('m,'n) t -> ('m,'n) t -> ('m,'n) t
val ( *  ) : ('m,'n) t -> ('n,'o) t -> ('m,'o) t
val ( *. ) : float     -> ('m,'n) t -> ('m,'n) t

val neg  : ('m,'n) t -> ('m,'n) t
val add  : ('m,'n) t -> ('m,'n) t -> ('m,'n) t
val sub  : ('m,'n) t -> ('m,'n) t -> ('m,'n) t
val mul  : ('m,'n) t -> ('n,'o) t -> ('m,'o) t
val smul : float -> ('m,'n) t -> ('m,'n) t

val invert : ('n,'n) t -> ('n,'n) t
val transpose : ('m,'n) t -> ('n,'m) t
(* val det : ('n,'n) t -> float *)

(** {6 Predicates} *)
(*----------------------------------------------------------------------------*)

val is_equal : Float.eps -> ('m,'n) t -> ('m,'n) t -> bool
val is_zero  : Float.eps -> ('m,'n) t -> bool
val is_identity : Float.eps -> ('m,'n) t -> bool

(* (\** {6 Printing} *\) *)
(* (\*----------------------------------------------------------------------------*\) *)

(* val to_string : (_,_) t -> string *)
(* val print : Format.formatter -> (_,_) t -> unit *)
