(** Axis-aligned bounding-box of statically typed dimension*)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

type ('a,'n) t

(** {6 Helper types} *)
(*----------------------------------------------------------------------------*)

type cls = [ `Outside | `Inside ]

(** {6 Creation / Initialization} *)
(*----------------------------------------------------------------------------*)

val make : (_,'t) Vector.t -> (_,'t) Vector.t -> unit
val make_point : (_,'t) Vector.t -> unit

val nothing : 'n N.t -> (_,'n) t
val everything : 'n N.t -> (_,'n) t

val nothing' : (ephemeral,'n) t -> unit
val everything': (ephemeral,'n) t -> unit

val random_gaussian : ?state:Rnd.state -> ?mu:float -> ?sigma:float -> 'n N.t -> (_,'n) t

(** {6 Conversion and copying} *)
(*----------------------------------------------------------------------------*)

val copied : (_,'n) t -> (_,'n) t

val copy' : (_,'n) t -> (ephemeral,'n) t -> unit

(** {6 Access} *)
(*----------------------------------------------------------------------------*)

val get_lower' : (_,'n) t -> (ephemeral,'n) Vector.t -> unit
val get_upper' : (_,'n) t -> (ephemeral,'n) Vector.t -> unit
val get_delta' : (_,'n) t -> (ephemeral,'n) Vector.t -> unit

val lower : (persistent,'n) t -> (persistent,'n) Vector.t
val upper : (persistent,'n) t -> (persistent,'n) Vector.t
val delta : (_,'n) t -> (_,'n) Vector.t

(** {6 Representation} *)
(*----------------------------------------------------------------------------*)

(** Escape local mutability. *)
val __absolve__ : (ephemeral,'n) t -> (_,'n) t
(** Phantom-type magic *)
val __magic__ : (_,'n) t -> (_,'n) t

(** {6 Ephemeral operations} *)
(*------------------------------------*)

val union' : (_,'n) t -> (_,'n) t -> (ephemeral,'n) t -> unit
val intersection' : (_,'n) t -> (_,'n) t -> (ephemeral,'n) t -> unit

val embrace_value' : (ephemeral,'n) t -> (_,'n) Vector.t -> unit
(** Clamping with nothing is undefined *)
val clamp_value' : (_,'n) t -> (_,'n) Vector.t -> (ephemeral,'n) Vector.t -> unit

(** {6 Pure operations} *)
(*----------------------------------------------------------------------------*)

val union : (_,'n) t -> (_,'n) t -> (_,'n) t -> unit
val intersection : (_,'n) t -> (_,'n) t -> (_,'n) t -> unit

val embrace_value : (_,'n) t -> (_,'n) Vector.t -> (_,'n) t
(** Clamping with nothing is undefined *)
val clamp_value : (_,'n) t -> (_,'n) Vector.t -> (_,'n) Vector.t

val is_inside : (_,'n) t -> (_,'n) Vector.t -> bool

(** {6 Printing} *)
(*----------------------------------------------------------------------------*)

val print : Format.formatter -> (_,_) t -> unit
val to_string : (_,_) t -> string

(** {6 Predicates} *)
(*----------------------------------------------------------------------------*)

val intersects : eps:float -> (_,'n) t -> (_,'n) t -> bool
val contains : eps:float -> (_,'n) t -> (_,'n) t -> bool

val is_equal : eps:float -> (_,'n) t -> (_,'n) t -> bool

val is_nothing : (_,'n) t -> bool
val is_everything : (_,'n) t -> bool

(** {6 Unit testing} *)
(*----------------------------------------------------------------------------*)

val test_equal : string -> (_,'n) t -> (_,'n) t -> unit
val unit_test : OUnit.test

