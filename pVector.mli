(* Kaspar Rohrer, Sat Mar  6 02:13:48 CET 2010 *)

(** Polymorphic vector

    Vector should be polymorphic because float arrays are handled
    specially.
*)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

type ('a,'b,'n) t

(** {6 Creation / Initialization} *)
(*----------------------------------------------------------------------------*)

val make : 'n N.t -> 'a -> (_,'a,'n) t
val make1 : 'a -> (_,'a,N._1) t
val make2 : 'a -> 'a -> (_,'a,N._2) t
val make3 : 'a -> 'a -> 'a -> (_,'a,N._3) t
val make4 : 'a -> 'a -> 'a -> 'a -> (_,'a,N._4) t

val fill' : 'a -> (ephemeral,'a,'n) t -> unit

(** {6 Conversion and copying} *)
(*----------------------------------------------------------------------------*)

val copy_to_array' : (_,'a,_) t -> 'a array -> unit
val copy_from_array' : (ephemeral,'a,_) t -> 'a array -> unit

val copied : (_,'a,'n) t -> (_,'a,'n) t

val copy' : (_,'a,'n) t -> (ephemeral,'a,'n) t -> unit

val xfer' : (_,'a,'s) t -> (ephemeral,'a,'n) t -> unit
(** Copies the elements in common fields between vectors of different size*)

(** {6 Representation} *)
(*----------------------------------------------------------------------------*)

val of_array : 'n N.t -> 'a array -> (ephemeral,'a,'n) t
val to_array : (ephemeral,'a,'n) t -> 'a array

val __absolve__ : (ephemeral,'a,'n) t -> (_,'a,'n) t
(** Escape from local mutability. *)

val __magic__ : (_,'a,'n) t -> (_,'a,'n) t
(** Phantom-type magic *)

(** {6 Access} *)
(*----------------------------------------------------------------------------*)

val count : (_,_,_) t -> int

val dim : (_,_,_) t -> int
val dimension : (_,_,'n) t -> 'n N.t

val get : (_,'a,_) t -> int -> 'a
val set' : (ephemeral,'a,_) t -> int -> 'a -> unit

(** {6 Transformation} *)
(*----------------------------------------------------------------------------*)

val iter : ('a -> unit) -> (_,'a,_) t -> unit
val fold_left : ('a -> 'b -> 'a) -> 'a -> (_,'b,_) t -> 'a
val fold_right : ('a -> 'b -> 'b) -> (_,'a,_) t -> 'b -> 'b
val map : ('a -> 'b) -> (_,'a,'n) t -> (_,'b,'n) t

val map' : ('a -> 'a) -> (_,'a,'n) t -> (ephemeral,'a,'n) t -> unit

(** {6 Printing} *)
(*----------------------------------------------------------------------------*)

val to_string : ?pr:(Format.formatter -> 'a -> unit) -> (_,'a,_) t -> string
val print : ?pr:(Format.formatter -> 'a -> unit) -> Format.formatter -> (_,'a,_) t -> unit

(** {6 Unit testing} *)
(*----------------------------------------------------------------------------*)

val test_equal : ?pr:(Format.formatter -> 'a -> unit) -> string -> (_,'a,'n) t -> (_,'a,'n) t -> unit
val unit_test : OUnit.test
