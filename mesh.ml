(* $Id$ *)

(*----------------------------------------------------------------------------*)

type readwrite = Core.readwrite
type const = Core.const

type 'a coords3 = 'a TinyLin.vector3

type ('a,'e) set = readwrite Set.t

(*----------------------------------------------------------------------------*)

type edge = {
  edge_from : Vertex.t;
  edge_to : Vertex.t;
  edge_attributes : readwrite;
  edge_adjacent_triangles : (readwrite,Triangle.t) set;
}

module Vertex =
struct
  open TinyLin

  type t = { 
    coordinates : readwrite vector3;
    mutable adjacent : t list
  }

  let make coords adjacent =
    {
      coords = V.copied coords;
      adjacent = adjacent
    }
end

module Edge =
struct
end

module Triangle =
struct
end

module Tetrahedron =
struct
end


 type edge = Edge.t
type vertex = Vertex.t
type triangle = Triangle.t
type tetrahedron = Tetrahedron.t


type vertex = int
type triangle = int
type tetrahedron = int

type 'a attribute

type +'a mesh =
{
  edges : (vertex * vertex) Sparse.array;
  vertices : vertex Sparse.array

  mutable edges : (vertex * vertex) array;
  mutable vertices : (readwrite coords3) array; 
  mutable triangles : (vertex * vertex * vertex) array;
  mutable tetrahedrons : (vertex * vertex * vertex * vertex) array;
  mutable free_edges : edge list;
  mutable free_vertices : vertex list;
  mutable free_triangles : triangle list;
  mutable free_tetrahedrons : tetrahedron list;
  mutable edge_count : int;
  mutable vertex_count : int;
  mutable triangle_count : int;
  mutable tetrahedron_count : int
}

(*----------------------------------------------------------------------------*)

let compact m =
  

val collapse_edge : readwrite mesh -> edge -> unit
val collapse_vertex : readwrite mesh -> vertex -> unit
val collapse_triangle : readwrite mesh -> triangle -> unit
val collapse_tetrahedron : readwrite mesh -> tetrahedron -> unit

val add_edge : 
  readwrite mesh -> vertex -> vertex -> unit
val add_vertex : 
  readwrite mesh -> vector3 -> vertex -> unit
val add_triangle : 
  readwrite mesh -> vertex -> vertex -> vertex -> triangle -> unit
val add_tetrahedron : 
  readwrite mehs -> verte -> vertex -> vertex -> vertex -> tet

val remove_edge :
  readwrite mesh -> edge -> unit
val remove_vertex :
  readwrite mesh -> vertex -> unit
val remove_triangle :
  readwrite mesh -> triangle -> unit
val remove_tetrahedron :
  readwrite mesh -> tetrahedron -> unit

val new_attribute : readwrite mesh -> 'a -> 'a attribute
val delete_attribute : readwrite mesh -> 'a attribute -> unit

val set_edge_attribute : 
  readwrite mesh -> edge -> 'a attribute -> 'a -> unit
val set_vertex_attribute : 
  readwrite mesh -> vertex -> 'a attribute -> 'a -> unit
val set_triangle_attribute :
  readwrite mesh -> triangle -> 'a attribute -> 'a -> unit
val set_tetrahedron_attribute :
  readwrite mesh -> tetrahedron -> 'a attribute -> 'a -> unit

(*----------------------------------------------------------------------------*)
