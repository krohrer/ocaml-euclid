(** {6 Unsafe mutable operations on matrices}

    Column-major. So
    
     /a b c\
    | d e f | = A
     \g h i/

    looks like this in memory:

    A = (a b c d e f g h i)

    A = a(i)(k)

    a(0)(0) = a
    a(0)(2) = c
    a(2)(0) = g
    a(2)(2) = i

    These operations are supposed to be used as building blocks for
    side-effect free operations, in order to keep unnecessary
    allocations down. I'm pretty positive that this code performs
    slower than optimized C code. I'm just not sure whether it is worth
    the hassle to implement stuff in C yet. *)
(*----------------------------------------------------------------------------*)

exception NotInvertible

type t = float array
type vec = ImpVec.t

val alloc : m:int -> n:int -> t

val init : m:int -> n:int -> dst:t -> (int -> int -> float) -> unit
val fill : m:int -> n:int -> dst:t -> float -> unit
val eye : m:int -> n:int -> dst:t -> unit

val get : m:int -> n:int -> t -> int -> int -> float
val set : m:int -> n:int -> dst:t -> int -> int -> float -> unit

val set_col : m:int -> n:int -> dst:t -> int -> vec -> unit
val set_row : m:int -> n:int -> dst:t -> int -> vec -> unit
val set_diag : m:int -> dst:t -> vec -> unit

val neg : m:int -> n:int -> dst:t -> t -> unit
val add : m:int -> n:int -> dst:t -> t -> t -> unit
val sub : m:int -> n:int -> dst:t -> t -> t -> unit

(* [mul .. a b] = A * B
   Multiplication of M.N with N.O matrix to yield M.O *)
val mul : dst:t -> m:int -> n:int -> t -> o:int -> t -> unit

(* [smul .. s a] = a * M 
   Scalar multiplication of M.N matrix *)
val smul  : dst:t -> float -> m:int -> n:int -> t -> unit

(* [mulv .. a v] = M * v 
   Multiplication of M.N matrix with N vector. *)
val mulv  : dst:vec -> m:int -> n:int -> t -> vec -> unit

(* [tvmul .. v a] = v^T * A
   Multiplication of transposed N vector (1.N matrix) with M.N
   matrix. *)
val tvmul : dst:vec -> vec -> m:int -> n:int -> t -> unit

(* [tmulv .. a v] = A^T * v = v^T * A
   Multiplication of transposed M.N matrix (N.M matrix ) with N
   vector. *)
val tmulv : dst:vec -> m:int -> n:int -> t -> vec -> unit

(* [transpose .. a] = A^T

   Transposition of M.N matrix into N.M matrix *)
val transpose : m:int -> n:int -> dst:t -> t -> unit

(* [invert .. a] = A^-1 *)
val invert : m:int -> dst:t -> t -> unit

val dump : m:int -> n:int -> t -> unit

val is_equal : Float.eps -> m:int -> n:int -> t -> t -> bool
val is_zero : Float.eps -> m:int -> n:int -> t -> bool
val is_identity : Float.eps -> m:int -> n:int -> t -> bool
