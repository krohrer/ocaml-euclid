(** {6 Option} *)

module Option :
sig
  type 'a t = 'a option

  val maybe : ('a -> unit) t -> 'a -> unit
  val may : ('a -> 'b) -> 'a t -> unit
  val may_map : ('a -> 'b) -> 'a t -> 'b t
  val default : 'a -> 'a t -> 'a
  val may_default : ('a -> 'b) -> 'a -> 'b t -> 'b

  val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

(** {6 Fast float arrays} *)

val alloc_float_array : int -> float array

(** {6 Base types} *)


