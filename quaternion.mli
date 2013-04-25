(* Kaspar Rohrer, Tue Jan 20 02:32:31 CET 2009 *)

(** Quaternions (persistent/ephemeral) *)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

type 'a t

(*----------------------------------------------------------------------------*)
(** Creation / Initialization *)
(*----------------------------------------------------------------------------*)

val make : ?real:float -> ?i:float -> ?j:float -> ?k:float -> unit -> _ t

val axis_rotation : (_,N._3) Vector.t -> Angle.t -> _ t
val axis_rotation' : (_,N._3) Vector.t -> Angle.t -> ephemeral t -> unit

val zero : persistent t
val identity : persistent t

val zero' : ephemeral t -> unit
val identity' : ephemeral t -> unit

val ei : persistent t
val ej : persistent t
val ek : persistent t

val ei' : ephemeral t -> unit
val ej' : ephemeral t -> unit
val ek' : ephemeral t -> unit

val random_unit : ?state:Rnd.state -> unit -> _ t

(*----------------------------------------------------------------------------*)
(** Conversion and copying *)
(*----------------------------------------------------------------------------*)

val to_matrix' : _ t -> (ephemeral,N._3,N._3) Matrix.t -> unit
val to_matrix : _ t -> (_,N._3,N._3) Matrix.t

val copy_to_array' : _ t -> float array -> unit
val copy_from_array' : ephemeral t -> float array -> unit

val copied : _ t -> _ t
val zeroed : _ t -> _ t

val copy' : _ t -> ephemeral t -> unit

(*----------------------------------------------------------------------------*)
(** Representation *)
(*----------------------------------------------------------------------------*)

val of_array : float array -> ephemeral t
val to_array : ephemeral t -> float array

val of_vector : ('a,N._4) Vector.t -> 'a t
val to_vector : 'a t -> ('a,N._4) Vector.t

(** Escape local mutability. The ephemeral reference should not be shared! *)
val __absolve__ : ephemeral t -> _ t
(** Phantom-type magik. Be very cautious with this! *)
val __magic__ : _ t -> _ t

(*----------------------------------------------------------------------------*)
(** Access *)
(*----------------------------------------------------------------------------*)

val real : _ t -> float
val imag : _ t -> (_,N._3) Vector.t
val i : _ t -> float
val j : _ t -> float
val k : _ t -> float

val get_imag' : _ t -> (ephemeral,N._3) Vector.t -> unit

val set4' : ephemeral t -> real:float -> i:float -> j:float -> k:float -> unit

val set_real' : ephemeral t -> float -> unit
val set_imag' : ephemeral t -> (_,N._3) Vector.t -> unit
val set_i' : ephemeral t -> float -> unit
val set_j' : ephemeral t -> float -> unit
val set_k' : ephemeral t -> float -> unit

(*----------------------------------------------------------------------------*)
(** Ephemeral operations *)
(*----------------------------------------------------------------------------*)

val neg'  : _ t -> ephemeral t -> unit
val sub'  : _ t -> _ t -> ephemeral t -> unit
val add'  : _ t -> _ t -> ephemeral t -> unit
val mul1' : _ t -> float -> ephemeral t -> unit
val mulq' : _ t -> _ t -> ephemeral t -> unit

val conjugate' : _ t -> ephemeral t -> unit
val inverse'   : _ t -> ephemeral t -> unit
val normalize' : _ t -> ephemeral t -> unit

val slerp' : t:float -> _ t -> _ t -> ephemeral t -> unit
 val concatenate' : _ t -> _ t -> ephemeral t -> unit

(*----------------------------------------------------------------------------*)
(** Pure operations *)
(*----------------------------------------------------------------------------*)

val neg  : _ t -> _ t
val sub  : _ t -> _ t -> _ t
val add  : _ t -> _ t -> _ t
val mul1 : _ t -> float -> _ t
val mulq : _ t -> _ t -> _ t

val conjugated : _ t -> _ t
val inversed   : _ t -> _ t
val normalized : _ t -> _ t

val slerped : t:float -> _ t -> _ t -> _ t
val concatenated : _ t -> _ t -> _ t

val magnitude : _ t -> float
val magnitude_squared : _ t -> float

(*----------------------------------------------------------------------------*)
(** Predicates *)
(*----------------------------------------------------------------------------*)

val is_equal : eps:float -> _ t -> _ t -> bool

(*----------------------------------------------------------------------------*)
(** Printing *)
(*----------------------------------------------------------------------------*)

val to_string : _ t -> string
val print : Format.formatter -> _ t -> unit

(*----------------------------------------------------------------------------*)
(** Unit testing *)
(*----------------------------------------------------------------------------*)

val unit_test : OUnit.test
val test_equal : string -> _ t -> _ t -> unit
