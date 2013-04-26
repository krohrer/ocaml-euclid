type ephemeral = Core.ephemeral
type persistent = Core.persistent

type ('a,'n) vtx_t = {
  v_point : ('a,'n) Vector.t;
  mutable v_neighbors : ('a,'n) vtx_t list
}

type ('a,'n) t = ('a,'n) vtx_t list

module type S =
sig
  type n
  val dim : n N.t
  val zero : (persistent,n) t

  val find_vertex : ('a,'a) t -> eps:float -> ('a,n) Vector.t -> ('a,n) vtx_t

  val make_vertex : ('a,n) Vector.t -> ('a,n) vtx_t

  val add_vertex : (persistent,n) t -> (persistent,n) vtx_t -> (persistent,n) vtx_t list -> (persistent,n) t
  val add_vertex' : (ephemeral,n) t -> (persistent,n) Vector.t -> (ephemeral,n) vtx_t list -> unit

  val add_edge : (persistent,n) t -> (persistent,n) vtx_t -> (persistent,n) vtx_t -> (persistent,n) t
  val add_edge' : (ephemeral,n) t -> (ephemeral,n) vtx_t -> (ephemeral,n) vtx_t -> unit
end

module D3 : S with type n = N._3 =
struct
  type n = N._3
  let dim = N._3
  let zero = []

  let make_vertex p = {
    v_point = p;
    v_neighbors = []
  }

  let find_vertex ~eps p = 
    List.find (V.is_equal ~eps p) mesh

  let find_or_make_vertex ~eps p =
    try 
      find_vertex ~eps p
    with
	Not_found -> make_vertex p

  let add_edge mesh u v =
    

  let add_vertex mesh v neighbors =
    
end

(*----------------------------------------------------------------------------*)
