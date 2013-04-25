
(*----------------------------------------------------------------------------*)

type readwrite = Core.readwrite
type const = Core.const

type 'a vector3 = 'a TinyLin.vector3

type edge
type vertex
type triangle
type tetrahedron

type 'a attribute 

type +'a simplicial_mesh

(*----------------------------------------------------------------------------*)

val edges_from_vertex : _ mesh -> vertex -> edge list
val triangles_from_edge : _ mesh -> edge -> vertex list
val tetrahedrons_from_triangle : _ mesh -> triangle -> tetrahedron list

(* Removing an element of dimension D removes all adjacent elements of
   higher dimension (> D) as well. E.g. removing a vertex removes all
   edges, all triangles, all tetrahedrons ajacent to it; the remove
   operation cascades. For insertion of new elements, it is the other
   way around: If a new element of dimension D is inserted into the
   mesh, all its border elements of lower dimension (< D) will be
   created as well if they do not exist already.

   edge : vertex * vertex * triangle list
   vertex : vector * edge list
   triangle : vertex * vertex * vertex * tetrahedron list
   tetrahedron : vertex * vertex * vertex * vertex
*)

(*----------------------------------------------------------------------------*)

val compact : readwrite mesh -> unit

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
