(* $Id: escher.mli 41 2009-01-03 03:19:17Z krohrer $ *)

(*----------------------------------------------------------------------------*)

module Attributes :
sig
  type t

  val add_label : string -> t -> t
  val labels : t -> string list

  val zero : t
  val one : t

  val is_zero : t -> bool
  val is_one : t -> bool

  val intersects : other:t -> t -> bool
  val contains : other:t -> t -> bool

  val union : t -> t -> t
  val intersect : t -> t -> t
  val minus : t -> t -> t
end

type attr_t = Attributes.t

(*----------------------------------------------------------------------------*)

module Vertex :
sig
  type t

  val coords : t -> Vector3.t
end

type vertex_t = Vertex.t

(*----------------------------------------------------------------------------*)

module Edge :
sig
  type t

  val make : ?attributes:attr_t -> v_t -> v_t -> t
  val attributes : t -> attr_t

  val length : t -> float

  val vertex : int -> t -> v_t

  val v0 : t -> v_t
  val v1 : t -> v_t
end

type edge_t = Edge.t

(*----------------------------------------------------------------------------*)

module Triangle :
sig
  type t

  val make : ?attributes:attr_t -> v_t -> v_t -> v_t -> t
  val attributes : t -> attr_t

  val vertex : int -> t -> v_t

  val v0 : t -> v_t
  val v1 : t -> v_t
  val v2 : t -> v_t
end

type triangle_t = Triangle.t

(*----------------------------------------------------------------------------*)

module Tetrahedron :
sig
  type t

  val make : ?attributes:attr_t -> v_t -> v_t -> v_t -> v_t -> t
  val attributes : t -> attr_t

  val vertex : int -> t -> v_t

  val v0 : t -> v_t
  val v1 : t -> v_t
  val v2 : t -> v_t
  val v3 : t -> v_t
end

type tetrahedron_t = Triangle.t

(*----------------------------------------------------------------------------*)

module World :
sig
  type prim_t = [
  | `Vertex of Vertex.t
  | `Edge of Edge.t
  | `Triangle of Triangle.t
  | `Tetrahedron of Tetrahedron.t
  ]

  type 'a soup_t = 'a list
  type t

  val insert' : t -> prim_t soup_t -> unit
  val remove' : t -> prim_t soup_t -> unit

  (* how do attributes come into play here? *)

  type predicate : prim_t -> attr_t -> bool
  type crawler : prim -> prim list

  val select : predicate -> crawler -> t -> prim soup
end

(*----------------------------------------------------------------------------*)

val map : (t -> t) -> t -> t
val fold : ('a -> t -> 'a) -> 'a -> t -> 'a

val mapc
  vertices    : (vertex_t      -> vertex_t     ) ->
  edges       : (edge_t        -> edge_t       ) ->
  triangle    : (triangle_t    -> triangle_t   ) ->
  tetrahedron : (tetrahedron_t -> tetrahedron_t) ->
  t -> t

val foldc
  vertices    : ('a -> vertex_t      -> 'a) ->
  edges       : ('a -> edge_t        -> 'a) ->
  triangle    : ('a -> triangle_t    -> 'a) ->
  tetrahedron : ('a -> tetrahedron_t -> 'a) ->
  'a -> t -> 'a

val volume : t -> float
val surface : t -> float

(*----------------------------------------------------------------------------*)
