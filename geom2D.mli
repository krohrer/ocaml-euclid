(* $Id: geom2.mli 10 2008-09-01 01:02:12Z krohrer $ *)

type vec = Vector2D.t
type mat = Matrix2D.t
type tform = Transform2D.t

(*----------------------------------------------------------------------------*)

module Point :
sig
  type t = vec

  val is_colinear_with : eps:float -> t -> t -> t -> bool
  val is_equal : eps:float -> t -> t -> bool

  val transform : tform -> t -> t
  
  val to_string : t -> string
end

(*----------------------------------------------------------------------------*)

module Line :
sig
  type t

  val make : Point.t -> Point.t -> t

  val fst : t -> Point.t
  val snd : t -> Point.t
  val dir : t -> vec

  val reverse : t -> t

  val map : (Point.t -> Point.t) -> t -> t
  val iter : (Point.t -> unit) -> t -> unit
  val fold : ('a -> Point.t -> 'a) -> 'a -> t -> 'a
  
  val distance_to_point : t -> Point.t -> float
  
  val is_equal : eps:float -> t -> t -> bool

  val transform : tform -> t -> t

  val to_string : t -> string
end

(*----------------------------------------------------------------------------*)

module Halfspace :
sig
  type t

  val make : float -> n:vec -> t
  val make_anchored : Point.t -> n:vec -> t
  val make_from_points : Point.t -> Point.t -> t

  val normal : t -> vec
  val anchor : t -> Point.t
  val offset : t -> float
  val point_offset : t -> Point.t -> float

  val classify_point : eps:float -> t -> Point.t -> FTest.t
  val classify_points : eps:float -> t -> Point.t list -> FTest.space
  val project_point : t -> Point.t -> Point.t
  val mirror_point : t -> Point.t -> Point.t
  
  val intersect2 : t -> t -> Point.t
  val intersect_line : t -> Line.t -> Point.t

  val shift : float -> t -> t
  val invert : t -> t
  val transform : tform -> t -> t

  val to_string : t -> string
end

(*----------------------------------------------------------------------------*)

module Polygon :
sig
  type t

  val make : ?hole:bool -> Point.t array -> t

  val empty : t
  val is_empty : t -> bool

  val is_hole : t -> bool

  val length : t -> int
  val vertex : t -> int -> Point.t
  val vertices : t -> Point.t array

  val edge : t -> int -> Line.t
  val edges : t -> Line.t list

  val reverse : t -> t

  val of_list : ?hole:bool -> Point.t list -> t
  val to_list : t -> Point.t list
  
  val map : (Point.t -> Point.t) -> t -> t
  val iter : (Point.t -> unit) -> t -> unit
  val fold : ('a -> Point.t -> 'a) -> 'a -> t -> 'a

  val flatten : Halfspace.t -> t -> t
  val split : Halfspace.t -> t -> (t*t)

  val signed_area : ?clip_x:float -> t -> float
  val area : ?clip_x:float -> t -> float
 
  val transform : tform -> t -> t
  
  val to_string : t -> string
end

(*----------------------------------------------------------------------------*)

module PolygonSoup :
sig
  type t

  val empty : t
  val is_empty : t -> bool

  val make : Polygon.t list -> t

  val length : t -> int
  val add : Polygon.t -> t -> t

  val map : (Polygon.t -> Polygon.t) -> t -> t
  val iter : (Polygon.t -> unit) -> t -> unit
  val fold : ('a -> Polygon.t -> 'a) -> 'a -> t -> 'a

  val polygons : t -> Polygon.t list

  val flatten : Halfspace.t -> t -> t
  val split : Halfspace.t -> t -> (t*t)

  val signed_area : ?clip_x:float -> t -> float
  val area : ?clip_x:float -> t -> float

  val transform : tform -> t -> t

  val to_string : t -> string
end

(*----------------------------------------------------------------------------*)

module AABB :
sig
  type t

  val empty : t 
  val is_empty : t -> bool

  val contains : t -> t -> bool
  val intersects : t -> t -> bool
  
  val make : Point.t -> Point.t -> t
  val make_hull : Point.t list -> t
  val make_centered : Point.t -> size:vec -> t

  val min : t -> Point.t
  val max : t -> Point.t

  val x : t -> float * float
  val y : t -> float * float
  
  val center : t -> Point.t
  val size : t -> vec
  
  val add_point : t -> Point.t -> t
  val add_points : t -> Point.t list -> t
  val add_line : t -> Line.t -> t
  val add_lines : t -> Line.t list -> t
  val add_polygon : t -> Polygon.t -> t
  val add_polygon_soup : t -> PolygonSoup.t -> t

  val clamp_point : t -> Point.t -> Point.t
  val clamp : t -> t -> t

  val union : t -> t -> t
  val intersection : t -> t -> t

  val classify_point : t -> Point.t -> FTest.t
  val contains_point : t -> Point.t -> bool

  val contains : t -> t -> bool
  val intersects : t -> t -> bool
  
  val to_string : t -> string
end

(*----------------------------------------------------------------------------*)

module Polyhedron :
sig
  type t
  
  val make : Halfspace.t list -> t
  val make_box : AABB.t -> t

  val halfspaces : t -> Halfspace.t list
  
  val transform : tform -> t -> t
  
  val to_string : t -> string
end

(*----------------------------------------------------------------------------*)

module Frame :
sig
  type t

  val make : ?origin:Point.t -> ?x:vec -> ?y:vec -> unit -> t

  val identity : t
  val is_identity : t -> bool

  val of_transform : tform -> t
  val to_transform : t -> tform

  val to_gl : t -> GlMat.t

  val origin : t -> Point.t
  val x : t -> vec
  val y : t -> vec

  val transform : tform -> t -> t
  val move_to : Point.t -> t -> t
  val look_at : Point.t -> t -> t
  val translate : vec -> t -> t
  val rotate : angle:Angle.t -> t -> t

  val map_point : t -> Point.t -> Point.t
  val map_vector : t -> vec -> vec
  val map_normal : t -> vec -> vec
  val map_transform : t -> tform -> tform

  val inverse_map_point : t -> Point.t -> Point.t
  val inverse_map_vector : t -> vec -> vec
  val inverse_map_normal : t -> vec -> vec

  val to_string : t -> string
end

(*----------------------------------------------------------------------------*)
