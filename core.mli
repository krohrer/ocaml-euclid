(** {6 Pervasives} *)

exception Conversion of string
exception Break
exception Canceled

(** phantom type to indicate mutability *)
type ephemeral
(** phantom type to indicate immutability *)
type persistent

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
