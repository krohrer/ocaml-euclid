(* Kaspar Rohrer, Sat Mar  6 02:36:13 CET 2010 *)

(** Intervals (persistent/ephemeral) consist of a lower and an upper bound. Or nothing at all. *)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

type 'a t

(** {6 Helper types} *)
(*----------------------------------------------------------------------------*)

type cls = [`Out | `Eps | `In]

(** {6 Creation / Initialization} *)
(*----------------------------------------------------------------------------*)

val make : float -> float -> _ t
val make_point : float -> _ t

val nothing : persistent t
val everything : persistent t

val nothing' : ephemeral t -> unit
val everything': ephemeral t -> unit

val random_gaussian : ?state:Rnd.state -> ?mu:float -> ?sigma:float -> unit -> _ t

(** {6 Conversion and copying} *)
(*----------------------------------------------------------------------------*)

val copied : _ t -> _ t

val copy' : _ t -> ephemeral t -> unit

(** {6 Access} *)
(*----------------------------------------------------------------------------*)

val lower : _ t -> float
val upper : _ t -> float
val delta : _ t -> float

(** {6 Representation} *)
(*----------------------------------------------------------------------------*)

(** Escape local mutability. *)
val __absolve__ : ephemeral t -> _ t
(** Phantom-type magic *)
val __magic__ : _ t -> _ t

(** {6 Ephemeral operations} *)
(*----------------------------------------------------------------------------*)

val union' : _ t -> _ t -> ephemeral t -> unit
val intersection' : _ t -> _ t -> ephemeral t -> unit

val embrace_value' : _ t -> float -> ephemeral t -> unit
val grow' : _ t -> float -> ephemeral t -> unit

(** {6 Pure operations} *)
(*----------------------------------------------------------------------------*)

val union : persistent t -> persistent t -> persistent t
val intersection : persistent t -> persistent t -> persistent t

val embrace_value : _ t -> float -> _ t
val grow : _ t -> float -> persistent t

(** Clamping with nothing is undefined *)
val clamp_value : _ t -> float -> float

(** Find a value in the given domain for which the strictly increasing function [f]
    assumes the given floating point value. *)
val binary_search_strictly_increasing :
  ?n:int -> ?eps:float -> f:(float -> float) ->
  _ t -> float -> float

(* val classify_value : eps:float -> _ t -> float -> [`Nothing|`Below|`Lower|`Inside|`Upper|`Above] *)
(* val classify : eps:float -> _ t -> _ t -> [`Disjoint|`Contains|`Inside|`Equal|`Meets|`Covers|`Covered|`Overlap] *)

(** {6 Printing} *)
(*----------------------------------------------------------------------------*)

val print : Format.formatter -> _ t -> unit
val to_string : _ t -> string

(** {6 Predicates} *)
(*----------------------------------------------------------------------------*)

val intersects : _ t -> _ t -> bool
val contains : _ t -> _ t -> bool

val contains_value : _ t -> float -> bool

val is_equal : eps:float -> _ t -> _ t -> bool

val is_nothing : _ t -> bool
val is_everything : _ t -> bool

(** {6 Unit testing} *)
(*----------------------------------------------------------------------------*)

val test_equal : string -> _ t -> _ t -> unit
val unit_test : OUnit.test

