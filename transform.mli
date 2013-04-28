(** Affine transformations (ephemeral/persistent) with statically typed dimensions *)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

type ('a,'m,'n) t

(*----------------------------------------------------------------------------*)
(** {6 Creation / Initialization} *)
(*----------------------------------------------------------------------------*)

val make : 'm N.t -> 'n N.t -> (_,'m,'n) t
val make_from : linear:(persistent,'m,'n) Matrix.t -> translation:(persistent,'m) Vector.t -> (persistent,'m,'n) t

val identity : 'n N.t -> (_,'n,'n) t
val identity' : (ephemeral,'n,'n) t -> unit

(*----------------------------------------------------------------------------*)
(** {6 Representation} *)
(*----------------------------------------------------------------------------*)

(** Escape from local mutability. *)
val __absolve__ : (ephemeral,'m,'n) t -> (_,'m,'n) t
(** Phantom-type magic *)
val __magic__ : (_,'m,'n) t -> (_,'m,'n) t

(*----------------------------------------------------------------------------*)
(** {6 Access} *)
(*----------------------------------------------------------------------------*)

val source_dimension : (_,_,'n) t -> 'n N.t
val target_dimension : (_,'m,_) t -> 'm N.t

val get_translation' : (_,'m,_) t -> (ephemeral,'m) Vector.t -> unit
val set_translation' : (ephemeral,'m,_) t -> (_,'m) Vector.t -> unit
val get_linear' : (_,'m,'n) t -> (ephemeral,'m,'n) Matrix.t -> unit
val set_linear' : (ephemeral,'m,'n) t -> (_,'m,'n) Matrix.t -> unit

val translation : (persistent,'m,_) t -> (persistent,'m) Vector.t
val linear : (persistent,'m,'n) t -> (persistent,'m,'n) Matrix.t

(*----------------------------------------------------------------------------*)
(** {6 Ephemeral operations} *)
(*----------------------------------------------------------------------------*)

val invert' : (_,'n,'n) t -> (ephemeral,'n,'n) t -> unit
val concatenate' : (_,'l,'n) t -> (_,'m,'l) t -> (ephemeral,'m,'n) t -> unit

val transform_point' : (_,'m,'n) t -> (_,'n) Vector.t -> (ephemeral,'m) Vector.t -> unit
val transform_vector' : (_,'m,'n) t -> (_,'n) Vector.t -> (ephemeral,'m) Vector.t -> unit
val transform_normal' : (_,'n,'n) t -> (_,'n) Vector.t -> (ephemeral,'n) Vector.t -> unit
val inv_transform_point' : (_,'n,'n) t -> (_,'n) Vector.t -> (ephemeral,'n) Vector.t -> unit
val inv_transform_vector' : (_,'n,'n) t -> (_,'n) Vector.t -> (ephemeral,'n) Vector.t -> unit
val inv_transform_normal' : (_,'n,'n) t -> (_,'n) Vector.t -> (ephemeral,'n) Vector.t -> unit

(*----------------------------------------------------------------------------*)
(** {6 Pure operations} *)
(*----------------------------------------------------------------------------*)

val inverted : (persistent,'n,'n) t -> (persistent,'n,'n) t
val concatenated : (_,'l,'n) t -> (_,'m,'l) t -> (_,'m,'n) t

val transformed_point : (_,'m,'n) t -> (_,'n) Vector.t -> (_,'m) Vector.t
val transformed_vector : (_,'m,'n) t -> (_,'n) Vector.t -> (_,'m) Vector.t
val transformed_normal : (_,'n,'n) t -> (_,'n) Vector.t -> (_,'n) Vector.t
val inv_transformed_point : (_,'n,'n) t -> (_,'n) Vector.t -> (_,'n) Vector.t
val inv_transformed_vector : (_,'n,'n) t -> (_,'n) Vector.t -> (_,'n) Vector.t
val inv_transformed_normal : (_,'n,'n) t -> (_,'n) Vector.t -> (_,'n) Vector.t

(*----------------------------------------------------------------------------*)
(** {6 Predicates} *)
(*----------------------------------------------------------------------------*)

val is_equal : eps:float -> (_,'m,'n) t -> (_,'m,'n) t -> bool

(*----------------------------------------------------------------------------*)
(** {6 Printing} *)
(*----------------------------------------------------------------------------*)

val to_string : (_,_,_) t -> string
val print : Format.formatter -> (_,_,_) t -> unit

(*----------------------------------------------------------------------------*)
(** {6 Unit testing} *)
(*----------------------------------------------------------------------------*)

val test_equal : string -> (_,'m,'n) t -> (_,'m,'n) t -> unit
val unit_test : OUnit.test

