(* $Id: geom3.mli 10 2008-09-01 01:02:12Z krohrer $ *)

type vec = Vector3D.t
type matrix = Matrix3D.t
type transform = Transform3D.t

(*----------------------------------------------------------------------------*)

module Point :
sig
  type t = vec

  val to_2d : t -> Geom2D.Point.t
  val of_2d : ?z:float -> Geom2D.Point.t -> t

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

  val to_2d : t -> Geom2D.Line.t
  val of_2d : ?z:float -> Geom2D.Line.t -> t

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
  val make_from_points : Point.t -> Point.t -> Point.t -> t

  val to_2d : t -> Geom2D.Halfspace.t
  val of_2d : Geom2D.Halfspace.t -> t

  val normal : t -> vec
  val anchor : t -> Point.t
  val offset : t -> float
  val point_offset : t -> Point.t -> float

  val classify_point : eps:float -> t -> Point.t -> FTest.t
  val classify_points : eps:float -> t -> Point.t list -> FTest.space
  val project_point : t -> Point.t -> Point.t
  val mirror_point : t -> Point.t -> Point.t
  
  val intersect2 : t -> t -> Line.t
  val intersect3 : t -> t -> t -> vec
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
    = { hole : bool;
	vertices : Point.t array }

  val make : ?hole:bool -> Point.t array -> t

  val empty : t
  val is_empty : t -> bool
  val is_hole : t -> bool

  val set_hole : bool -> t -> t

  val uniform_z : t -> float option

  val to_2d : t -> Geom2D.Polygon.t
  val of_2d : ?z:float -> Geom2D.Polygon.t -> t

  val length : t -> int
  val vertex  :t -> int -> Point.t
  val vertices : t -> Point.t array

  val edge : t -> int -> Line.t
  val edges : t -> Line.t list
  val rev_edges : t -> Line.t list

  val reverse : t -> t

  val of_list : ?hole:bool -> Point.t list -> t
  val to_list : t -> Point.t list
  
  val map : (Point.t -> Point.t) -> t -> t
  val iter : (Point.t -> unit) -> t -> unit
  val fold : ('a -> Point.t -> 'a) -> 'a -> t -> 'a
  val fold_edges : ('a -> Point.t -> Point.t -> 'a) -> 'a -> t -> 'a

  val flatten : Halfspace.t -> t -> t
  val split : Halfspace.t -> t -> (t*t)

  val signed_area_2d : ?clip_x:float -> t -> float
  val area_2d : ?clip_x:float -> t -> float

  val is_clockwise_2d : t -> bool
  val is_counter_clockwise_2d : t -> bool

  val make_slab_from_halfspace : ccw:bool -> dim:float -> Halfspace.t -> t
  
  val transform : tform -> t -> t
  
  val to_string : t -> string
end

(*----------------------------------------------------------------------------*)

module PolygonSoup :
sig
  type t
    = Polygon.t list

  val make : Polygon.t list -> t

  val empty : t
  val is_empty : t -> bool

  val uniform_z : t -> float option

  val to_2d : t -> Geom2D.PolygonSoup.t
  val of_2d : ?z:float -> Geom2D.PolygonSoup.t -> t

  val add : Polygon.t -> t -> t

  val map : (Polygon.t -> Polygon.t) -> t -> t
  val iter : (Polygon.t -> unit) -> t -> unit
  val fold : ('a -> Polygon.t -> 'a) -> 'a -> t -> 'a
  val filter : (Polygon.t -> bool) -> t -> t

  val length : t -> int
  val polygons : t -> Polygon.t list

  val flatten : Halfspace.t -> t -> t
  val split : Halfspace.t -> t -> (t*t)

  val signed_area_2d : ?clip_x:float -> t -> float
  val area_2d : ?clip_x:float -> t -> float

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
  val make_centered : Point.t -> size:vec -> t
  val make_hull : Point.t list -> t

  val to_2d : t -> Geom2D.AABB.t
  val of_2d : ?z:float*float -> Geom2D.AABB.t -> t

  val min : t -> Point.t
  val max : t -> Point.t
  
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

(* FIXME : what are the semantics of rotation and translation exactly?
 * -> also see TransformX and Geom2D.Frame Transform seems to use the
 * same semantics as the OpenGL Matrix commands.  However, for frames,
 * transform and rotate should be understood to transform the axes and
 * origin of the frame, and not the local coordinate system. One solution
 * would be to remove the `translate' and `rotate'
 * functions. `transform_locally' should probably be renamed or removed,
 * also. and `move_to' should be `set_origin' instead.  another
 * problematic function is `look_at' and `look_at_local', because they
 * assume that the x-axis is the eye vector, which is not always true. It
 * would be better to explicitly give an eye vector and rotate the frame
 * appropriately so that the new eye vector faces the given
 * direction. Anyway, improvements for future versions.  *)
  
module Frame :
sig
  type t

  val make : ?origin:Point.t -> ?x:vec -> ?y:vec -> ?z:vec -> unit -> t

  val identity : t
  val is_identity : t -> bool

  val of_transform : tform -> t
  val to_transform : t -> tform

  val to_gl : t -> GlMat.t

  val origin : t -> Point.t
  val x : t -> vec
  val y : t -> vec
  val z : t -> vec

  val transform : tform -> t -> t
  val transform_locally : tform -> t -> t
  val move_to : Point.t -> t -> t
  val look_at : Point.t -> t -> t
  val look_at_local : Point.t -> t -> t
  val translate : vec -> t -> t
  val rotate : angle:Angle.t -> vec -> t -> t

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
