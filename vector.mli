(** Vector (persistent/ephemeral) of floats of statically typed
    dimension *)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

type ('a,'n) t

(** {6 Helper types} *)
(*----------------------------------------------------------------------------*)

(** 1D Swizzling *)
type cswz1 = [`x|`nx]
(** 2D Swizzling *)
type cswz2 = [`y|`ny|cswz1]
(** 3D Swizzling *)
type cswz3 = [`z|`nz|cswz2]
(** 4D Swizzling *)
type cswz4 = [`w|`nw|cswz3]

(** {6 Creation / Initialization} *)
(*----------------------------------------------------------------------------*)

(** [V.make ?init dim] creates a new vector of dimension [dim] with
    its elements set to [init] *)
val make : ?init:float -> 'n N.t -> (_,'n) t
val make1 : float -> (_,N._1) t
val make2 : float -> float -> (_,N._2) t
val make3 : float -> float -> float -> (_,N._3) t
val make4 : float -> float -> float -> float -> (_,N._4) t

val zero : 'n N.t -> (_,'n) t
val one : 'n N.t -> (_,'n) t

val random : ?state:Rnd.state -> ?lower:(_,'n) t -> ?upper:(_,'n) t -> 'n N.t -> (_,'n) t
val random_unit : ?state:Rnd.state -> 'n N.t -> (_,'n) t
val random_gaussian : ?state:Rnd.state -> ?mu:(_,'n) t -> ?sigma:(_,'n) t -> 'n N.t -> (_,'n) t

val fill' : float -> (ephemeral,_) t -> unit
val zero' : (ephemeral,_) t -> unit
val one'  : (ephemeral,_) t -> unit

val ex : (_ N.succ1 as 'n) N.t -> (_,'n) t
val ey : (_ N.succ2 as 'n) N.t -> (_,'n) t
val ez : (_ N.succ3 as 'n) N.t -> (_,'n) t
val ew : (_ N.succ4 as 'n) N.t -> (_,'n) t

val ex' : (ephemeral,_ N.succ1) t -> unit
val ey' : (ephemeral,_ N.succ2) t -> unit
val ez' : (ephemeral,_ N.succ3) t -> unit
val ew' : (ephemeral,_ N.succ4) t -> unit

(** {6 Conversion and copying} *)
(*----------------------------------------------------------------------------*)

val copy_to_array' : (_,_) t -> float array -> unit
val copy_from_array' : (ephemeral,_) t -> float array -> unit

val copied : (_,'n) t -> (_,'n) t
val zeroed : (_,'n) t -> (_,'n) t

val copy' : (_,'n) t -> (ephemeral,'n) t -> unit
(** Copies the elements in common fields between vectors of different size*)
val xfer' : (_,'s) t -> (ephemeral,'n) t -> unit

(** {6 Representation} *)
(*----------------------------------------------------------------------------*)

val of_array : 'n N.t -> float array -> (_,'n) t
val to_array : (_,'n) t -> float array

(** Escape from local mutability. *)
val __absolve__ : (ephemeral,'n) t -> (_,'n) t
(** Phantom-type magic *)
val __magic__ : (_,'n) t -> (_,'n) t

(** {6 Access} *)
(*----------------------------------------------------------------------------*)

val dim : (_,_) t -> int
val dimension : (_,'n) t -> 'n N.t

val get : (_,_) t -> int -> float
val set' : (ephemeral,_) t -> int -> float -> unit

val x : (_,_ N.succ1) t -> float
val y : (_,_ N.succ2) t -> float
val z : (_,_ N.succ3) t -> float
val w : (_,_ N.succ4) t -> float

val set_x' : (ephemeral,_ N.succ1) t -> float -> unit
val set_y' : (ephemeral,_ N.succ2) t -> float -> unit
val set_z' : (ephemeral,_ N.succ3) t -> float -> unit
val set_w' : (ephemeral,_ N.succ4) t -> float -> unit

(** {6 Ephemeral operations} *)
(*----------------------------------------------------------------------------*)

val neg'  : (_,'n) t -> (ephemeral,'n) t -> unit
val sub'  : (_,'n) t -> (_,'n) t -> (ephemeral,'n) t -> unit
val add'  : (_,'n) t -> (_,'n) t -> (ephemeral,'n) t -> unit
val mul1' : (_,'n) t -> float -> (ephemeral,'n) t -> unit
val mulv' : (_,'n) t -> (_,'n) t -> (ephemeral,'n) t -> unit
val min'  : (_,'n) t -> (_,'n) t -> (ephemeral,'n) t -> unit
val max'  : (_,'n) t -> (_,'n) t -> (ephemeral,'n) t -> unit

val mul1add' : (_,'n) t -> float -> (ephemeral,'n) t -> unit
val mulvadd' : (_,'n) t -> (_,'n) t -> (ephemeral,'n) t -> unit

val cross' : (_,N._3) t -> (_,N._3) t -> (ephemeral,N._3) t -> unit

val invert'   : (_,'n) t -> (ephemeral,'n) t -> unit
val normalize' : (_,'n) t -> (ephemeral,'n) t -> unit


val mid' : (_,'n) t -> (_,'n) t -> (ephemeral,'n) t -> unit
val lerp' : t:float -> (_,'n) t -> (_,'n) t -> (ephemeral,'n) t -> unit
val minmax' : (_,'n) t -> (_,'n) t -> (ephemeral,'n) t -> (ephemeral,'n) t -> unit

val swizzle2' : cswz2 -> cswz2 -> (_,N._2) t -> (ephemeral,N._2) t -> unit
val swizzle3' : cswz3 -> cswz3 -> cswz3 -> (_,N._3) t -> (ephemeral,N._3) t -> unit
val swizzle4' : cswz4 -> cswz4 -> cswz4 -> cswz4 -> (_,N._4) t -> (ephemeral,N._4) t -> unit

(** {6 Pure operations} *)
(*----------------------------------------------------------------------------*)

val neg  : (_,'n) t -> (_,'n) t
val sub  : (_,'n) t -> (_,'n) t -> (_,'n) t
val add  : (_,'n) t -> (_,'n) t -> (_,'n) t
val mul1 : (_,'n) t -> float -> (_,'n) t
val mulv : (_,'n) t -> (_,'n) t -> (_,'n) t
val min  : (_,'n) t -> (_,'n) t -> (_,'n) t
val max  : (_,'n) t -> (_,'n) t -> (_,'n) t

val mul1add : (_,'n) t -> float -> (_,'n) t
val mulvadd : (_,'n) t -> (_,'n) t -> (_,'n) t

val cross : (_,N._3) t -> (_,N._3) t -> (_,N._3) t

val inverted   : (_,'n) t -> (_,'n) t
val normalized : (_,'n) t -> (_,'n) t

val mid    : (_,'n) t -> (_,'n) t -> (_,'n) t
val lerped : t:float -> (_,'n) t -> (_,'n) t -> (_,'n) t
val minmax : (_,'n) t -> (_,'n) t -> (_,'n) t * (_,'n) t

val swizzled2 : cswz2 -> cswz2 -> (_,N._2) t -> (_,N._2) t
val swizzled3 : cswz3 -> cswz3 -> cswz3 -> (_,N._3) t -> (_,N._3) t
val swizzled4 : cswz4 -> cswz4 -> cswz4 -> cswz4 -> (_,N._3) t -> (_,N._3) t

val dot : (_,'n) t -> (_,'n) t -> float

val magnitude : (_,'n) t -> float
val magnitude_squared : (_,'n) t -> float

(** {6 Predicates} *)
(*----------------------------------------------------------------------------*)

val is_equal : eps:float -> (_,'n) t -> (_,'n) t -> bool
val is_uniform : (_,'n) t -> float -> bool

(** {6 Printing} *)
(*----------------------------------------------------------------------------*)

val to_string : (_,_) t -> string
val print : Format.formatter -> (_,_) t -> unit

(** {6 Unit testing} *)
(*----------------------------------------------------------------------------*)

val test_equal : string -> (_,'n) t -> (_,'n) t -> unit
val unit_test : OUnit.test

