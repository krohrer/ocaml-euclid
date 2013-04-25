(* $Id: geom3.ml 10 2008-09-01 01:02:12Z krohrer $ *)

module V = Vector3
module M = Matrix3x3
module T = Transform3

type vec = V.t
type mat = M.t
type tform = T.t

open Core
open FTest
(* open ExtList for great tail-recursion *)

(*----------------------------------------------------------------------------*)

let log = false
let vertex_snap = ldexp 1.0 (-10)

(*----------------------------------------------------------------------------*)

let split_eps = FNum.delta
let aabb_eps = FNum.delta

(*----------------------------------------------------------------------------*)

module Point =
struct
  type t = V.t

  let to_2d = V.to_2d
  let of_2d = V.of_2d

  let is_colinear_with ~eps x1 x2 x3 =
    let a = V.sub x2 x1 in
    let b = V.sub x3 x2 in
    let area = V.norm (V.cross a b) in
    let la = V.norm a in
    let lb = V.norm b in
      (* distance of shorter edge to line of longer edge *)
      if la > lb then 
	(* area / la < eps *)
	area < la *. eps
      else
	(* area / lb < eps *)
	area < lb *. eps

  let is_equal = V.is_equal

  let transform = T.transform_point

  let to_string = V.to_string
end

(*----------------------------------------------------------------------------*)

module Line =
struct
  module L2 = Geom2D.Line
  
  type t = Point.t * Point.t

  let make s e = s, e
  let fst (s, _) = s
  let snd (_, e) = e
  let dir (s, e) = V.sub e s

  let to_2d (s, e) = L2.make (Point.to_2d s) (Point.to_2d e)
  let of_2d ?z l = Point.of_2d ?z (L2.fst l), Point.of_2d ?z (L2.snd l)

  let reverse (s, e) = e, s

  let map f (s, e) = f s, f e
  let iter f (s, e) = f s; f e
  let fold f z (s, e) = f (f z s) e

  let distance_to_point (sl, el) p =
    let vl = V.sub el sl in
    let vp = V.sub el p in
    let area = V.norm (V.cross vl vp) in
      area /. (V.norm vl)
	
  let is_equal ~eps (s1, e1) (s2, e2) =
    let eq = Point.is_equal ~eps s1 s2 && Point.is_equal ~eps e1 e2 in
    let anti_eq = Point.is_equal ~eps s1 e2 && Point.is_equal ~eps s2 e1 in
      eq || anti_eq

  let transform at (sl, el) =
    Point.transform at sl, Point.transform at el
      
  let to_string (sl, el) =
    Printf.sprintf "[%s - %s]" (Point.to_string sl) (Point.to_string el)
end

(*----------------------------------------------------------------------------*)

module Halfspace =
struct
  module H2 = Geom2D.Halfspace

  type t = V.t * float
      (* (n,d): n dot (x,y,z) + d == 0, |n| = 1. *)
      
  let make offset ~n =
    let nf = 1./.V.norm n in
      (V.mul1 n nf, offset *. nf)

  let make_anchored anchor ~n =
    let n = V.normalize n in
      (n, ~-.(V.dot n anchor))

  let make_from_points p1 p2 p3 =
    let n = V.cross (V.sub p2 p1) (V.sub p3 p1) in
      make_anchored p1 ~n:n

  let normal (n, _) = n
  let anchor (n, d) = V.mul1 n ~-.d
  let offset (_, d) = d
  let point_offset (n, d) p = V.dot n p +. d

  let of_2d hs =
    let anchor = V.of_2d (H2.anchor hs) in
    let n = V.of_2d (H2.normal hs) in
      make_anchored anchor ~n

  let to_2d hs =
    let anchor = V.to_2d (anchor hs) in
    let n = V.to_2d (normal hs) in
      H2.make_anchored anchor ~n

  let classify_point ~eps h p =
    classify ~eps (point_offset h p)
      
  let classify_points ~eps h ps =
    let distances = List.map (point_offset h) ps in
      classify_fold ~eps EpsSpace distances
	
  let project_point (n, d) p =
    let dp = point_offset (n, d) p in
      V.sub p (V.mul1 n dp)
	
  let mirror_point (n, d) p =
    let dp = point_offset (n, d) p in
      V.sub p (V.mul1 n (2. *. dp))
	
  let intersect3 (n1, d1) (n2, d2) (n3, d3) =
    let ninv = M.inv (M.of_rows n1 n2 n3) in
    let d = V.neg (V.make d1 d2 d3) in
      M.mulv ninv d
	
  let intersect2 (n1, d1) (n2, d2) =
    let n3 = V.cross n1 n2 in
    let o = intersect3 (n1, d1) (n2, d2) (n3, 0.) in
      (o, V.add o n3)
	
  let intersect_line h (p1, p2) =
    let d1 = point_offset h p1 in
    let d2 = point_offset h p2 in
      (* (d1*p1 - d2*p2) / (d1-d2) *)
      V.div1 (V.sub (V.mul1 p1 d2) (V.mul1 p2 d1)) (d2 -. d1)

  let shift sh hs =
    make (offset hs -. sh) ~n:(normal hs)

  let invert (n, d) =
    V.neg n, ~-.d

  let transform at hs =
    let n = T.transform_normal at (normal hs) in
    let a = T.transform_point  at (anchor hs) in
      make_anchored a ~n:n

  let to_string (n, d) =
    Printf.sprintf "[%s . n + %f == 0]" (V.to_string n) d

end

(*----------------------------------------------------------------------------*)

module Polygon =
struct
  module P2 = Geom2D.Polygon

  type t =
      { hole : bool;
	vertices : Point.t array }

  module Opt =
  struct
    let vec_to_grid ~size gv =
      let f c = truncate (floor (0.5 +. c /. size)) in
      V.map' f gv  

    let vec_of_grid ~size gv =
      let f c = size *. float_of_int c in
      V.map' f gv

    let snap_vertices snap vs =
      let n = Array.length vs in
      let rec iter i gsucc res =
	if i > 0 then
	  let g = vec_to_grid ~size:snap vs.(i) in
	    if g = gsucc then
	      iter (i - 1) gsucc res
	    else
	      iter (i - 1) g ((vec_of_grid ~size:snap g)::res)
	else
	  Array.of_list res
      in
	if n > 0 then
	  iter (n - 1) (vec_to_grid ~size:snap vs.(0)) []
	else
	  [||]

    let merge_colinear_edges snap vs =
      let n = Array.length vs in
	if n > 1 then 
	  let vpred = ref vs.(0) in
	  let v     = ref vs.(n - 1) in
	  let vfirst = ref vs.(n - 1) in
	  let res = ref [] in
	    for i = n-1 downto 1 do
	      let vsucc = vs.(i - 1) in
		if not (Point.is_colinear_with ~eps:snap !vpred !v vsucc) then begin
		  if !res = [] then
		    vfirst := !v;
		  vpred := !v;
		  res := !v :: !res
		end;
		v := vsucc
	    done;
	    if Point.is_colinear_with ~eps:snap !vpred !v !vfirst then
	      ()
	    else
	      res := !v :: !res;
	    Array.of_list !res
	else
	  [||]

    let optimize_vertices vs =
      if true then
	let snap = vertex_snap in 
	  (* let vs = snap_vertices snap vs in *)
	let vs = merge_colinear_edges snap vs in
	  if Array.length vs < 3 then [||] else vs
      else
	vs
  end

  (*------------------------------------*)

  let make ?(hole=false) verts =
    let opt = { hole = hole; vertices = Opt.optimize_vertices verts } in
      if log then begin
	logf "Make polygon : org/opt : %d/%d\n"
	  (Array.length verts) (Array.length opt.vertices);
	logf "ORIGINAL ==============================\n";
	Array.iter (fun v -> Printf.printf "%s\n" (V.to_string v)) verts;
	logf "OPTIMIZED =============================\n";
	Array.iter (fun v -> Printf.printf "%s\n" (V.to_string v)) opt.vertices
      end;
      opt
      
  let empty = make [||]
  let is_empty p = p.vertices = [||]
  let is_hole p = p.hole

  let set_hole f p =
    { p with hole = f }

  let uniform_z p =
    let vs = p.vertices in
    let n = Array.length vs in
      if n = 0 then
	None
      else
	let z = V.z vs.(0) in
	let rec fold i =
	    if i < n then
	      if FNum.delta_compare z (V.z vs.(i)) = 0 then
		fold (i + 1)
	      else
		None
	    else
	      Some z
	in
	  fold 1

  let length p = Array.length p.vertices
  let vertex p i = p.vertices.(i)
  let vertices p = p.vertices

  let edge p i =
    let n = length p in
      vertex p (i mod n), vertex p ((i + 1) mod n)

  let edges p =
    let n = length p in
    let e = ref [] in
      for i = n-1 downto 0 do
	e := (edge p i)::!e
      done;
      !e

  let rev_edges p =
    List.rev_map Line.reverse (edges p)

  let reverse p =
    let n = length p in
    let v = vertices p in
    let v_rev = Array.make n V.zero in
      for i = 0 to n-1 do
	v_rev.(i) <- v.(n-1-i)
      done;
      make ~hole:(is_hole p) v_rev

  let of_list ?hole vl = make ?hole (Array.of_list vl)
  let to_list p = Array.to_list (vertices p)

  let to_2d p =
    P2.make
      ~hole:(is_hole p)
      (Array.map Point.to_2d (vertices p))

  let of_2d ?z p =
    make
      ~hole:(P2.is_hole p)
      (Array.map (Point.of_2d ?z) (P2.vertices p))
      
  let map f p = make ~hole:(is_hole p) (Array.map f (vertices p))
  let iter f p = Array.iter f (vertices p)
  let fold f z p = Array.fold_left f z (vertices p)

  let flatten halfspace p =
    map (Halfspace.project_point halfspace) p
      
  let split hs p =
    let eps = split_eps in
      (* iterate over edges and decide on each vertex, split as neccessary *)
      (* caution : reverses the order of the vertices *)
    let rec fold_rev (prev_vtx,prev_cls) (vneg,vpos) = function
      | [] -> (vneg,vpos)
      | vtx :: rest -> begin
	  let cls = classify ~eps (Halfspace.point_offset hs vtx) in
	    match prev_cls, cls with
	      | Positive, Negative ->
		  let vsplit = Halfspace.intersect_line hs (prev_vtx, vtx) in
		    (* FIXMAY : make sure point is on halfspace *)
		    (* let vsplit = Halfspace.project_point hs vsplit in *)
		  let vneg = vtx :: vsplit :: vneg in
		  let vpos =        vsplit :: vpos in
		    fold_rev (vtx, cls) (vneg, vpos) rest
	      | Negative, Positive ->
		  let vsplit = Halfspace.intersect_line hs (prev_vtx, vtx) in
		    (* FIXMAY : make sure point is on halfspace *)
		    (* let vsplit = Halfspace.project_point hs vsplit in *)
		  let vneg =        vsplit :: vneg in          
		  let vpos = vtx :: vsplit :: vpos in
		    fold_rev (vtx, cls) (vneg, vpos) rest
	      | _, Epsilon ->
		  let vneg = vtx :: vneg in
		  let vpos = vtx :: vpos in
		    fold_rev (vtx, cls) (vneg, vpos) rest
	      | _, Negative ->
		  let vneg = vtx :: vneg in
		    fold_rev (vtx, cls) (vneg, vpos) rest
	      | _, Positive ->
		  let vpos = vtx :: vpos in
		    fold_rev (vtx, cls) (vneg, vpos) rest
	end
    in
      if length p = 0 then
	empty, empty
      else
	let first_vtx = vertex p 0 in
	let first_cls = classify ~eps (Halfspace.point_offset hs first_vtx) in
	let neg, pos =
	  fold_rev
	    (first_vtx, first_cls)
	    ([], [])
	    (List.rev (to_list p))
	in
	let hole = is_hole p in
	  of_list ~hole neg, of_list ~hole pos

  let edges p =
    let n = length p in
    let e = ref [] in
      for i = n-1 downto 0 do
	e := (edge p i)::!e
      done;
      !e

  let fold_edges f zero p =
    let n = length p in
    let accu = ref zero in
      for i = 0 to n-1 do
	accu := f !accu (vertex p i) (vertex p ((i+1) mod n))
      done;
      !accu
	
  let signed_area_2d ?clip_x p =
    match clip_x with
      | None ->
	  let f a (x0, y0, _) (x1, y1, _) =
	    (* a +. (x0 *. y1 -. x1 *. y0) *)
	    a +. (x0 -. x1)*.(y0 +. y1)
	  in
	    0.5 *. (fold_edges f 0. p)
      | Some x ->
	  let f a (x0, y0, _) (x1, y1, _) =
	    if x <= min x0 x1 then
	      a
	    else if x >= max x0 x1 then
	      a +. (x0 -. x1)*.(y0 +. y1)
	    else
	      let y = y0 +. (y1 -. y0)*.(x -. x0)/.(x1 -. x0) in
		if x0 < x1 then
		  a +. (x0 -. x)*.(y0 +. y)
		else
		  a +. (x -. x1)*.(y +. y1)
	  in
	    0.5 *. (fold_edges f 0. p)

  let area_2d ?clip_x p =
    let a = abs_float (signed_area_2d ?clip_x p) in
      if is_hole p then
	~-. a
      else
	a

  let is_counter_clockwise_2d p =
    signed_area_2d p > 0.

  let is_clockwise_2d p = not (is_counter_clockwise_2d p)

  let make_slab_from_halfspace ~ccw ~dim halfspace =
    (* proto-slab *)
    let verts = [|1., 1., 0.; -1., 1., 0.; -1., -1., 0.; 1., -1., 0.|] in
      (* scale up proto-slab *)
    let verts = Array.map (V.mul (V.mul1 V.one dim)) verts in
      (* align proto-slab with major axis *)
    let (x,y,z), n = halfspace in
    let choose_swizzle (pm, ps) (m, s) =
      if m > pm then (m, s) else (pm, ps)
    in
    let _, swizzle =
      List.fold_left choose_swizzle (0.0, V.xyz)
	[x, V.zxy; -.x, V.zyx; y, V.yzx; -.y, V.xzy; z, V.xyz; -.z, V.yxz]
    in
    let verts = Array.map swizzle verts in
      (* drop prot-slab onto halfspace *)
    let project p = Halfspace.intersect_line halfspace (p, V.add p (swizzle V.ez)) in
    let slab = make (Array.map project verts) in
      (* cap slab at both ends to fit completely into universe *)
    let neg_cap = (swizzle (V.neg V.ez), -.dim) in
    let pos_cap = (swizzle        V.ez , -.dim) in
    let slab, _ = split neg_cap slab in
    let slab, _ = split pos_cap slab in
      if ccw then
	slab
      else
	reverse slab

  let transform am p =
    map (Point.transform am) p

  let to_string p =
    Printf.sprintf "Polygon[h?=%b, #v=%d]" (is_hole p) (length p)
end

(*----------------------------------------------------------------------------*)

module PolygonSoup =
struct
  module PS2 = Geom2D.PolygonSoup

  type t = Polygon.t list

  let empty = []
  let is_empty s = s = []

  let make pl = List.filter (fun p -> not (Polygon.is_empty p)) pl

  let uniform_z =
    function
      | [] -> None
      | x::rest ->
	  match Polygon.uniform_z x with
	    | None -> None
	    | Some z0 ->
		let rec iter =
		  function
		    | [] -> Some z0
		    | x::rest ->
			match Polygon.uniform_z x with
			  | None -> None
			  | Some z ->
			      if FNum.delta_compare z0 z = 0 then
				iter rest
			      else
				None
		in
		  iter rest

  let to_2d s = PS2.make (List.map Polygon.to_2d s)
  let of_2d ?z s = List.map (Polygon.of_2d ?z) (PS2.polygons s)

  let length s = List.length s
  let add p s = if not (Polygon.is_empty p) then p::s else s

  let map f s = make (List.map f s)
  let iter f s = List.iter f s
  let fold f z s = List.fold_left f z s
  let filter f s = List.filter f s

  let polygons s = s

  let flatten hs s = List.map (Polygon.flatten hs) s

  let signed_area_2d ?clip_x s =
    List.fold_left (fun a p -> a +. Polygon.signed_area_2d ?clip_x p) 0. s

  let area_2d ?clip_x s =
    List.fold_left (fun a p -> a +. Polygon.area_2d ?clip_x p) 0. s

  (*------------------------------------*)

  let split hs s =
    List.split (List.map (Polygon.split hs) s)

  (*------------------------------------*)

  let transform at s = 
    List.map (Polygon.transform at) s

  let to_string s =
    Printf.sprintf "PolygonSoup[#p=%d]" (List.length s)

end

(*----------------------------------------------------------------------------*)

module AABB =
struct
  module B2 = Geom2D.AABB

  let eps = aabb_eps

  type t = Point.t * Point.t

  let empty = (V.zero, V.zero)
  let is_empty b = b == empty

  let add_point b p =
    if is_empty b then
      (p, p)
    else
      let bmin, bmax = b in
	(V.min bmin p, V.max bmax p)

  let add_points box points =
    List.fold_left add_point box points

  let add_polygon b p =
    Polygon.fold add_point b p

  let add_polygon_soup b s =
    List.fold_left add_polygon b s

  let add_line b l =
    add_points b [Line.fst l; Line.snd l]

  let add_lines b ls =
    List.fold_left add_line b ls

  let make p1 p2 =
    add_point (p1, p1) p2

  let make_hull points =
    add_points empty points

  let make_hull =
    function
      | [] -> empty
      | p::rest -> add_points (p,p) rest

  let make_centered center ~size =
    let hsize = V.mul1 size 0.5 in
      make (V.sub center hsize) (V.add center hsize)

  let to_2d (bmin, bmax) = B2.make (Point.to_2d bmin) (Point.to_2d bmax)
  let of_2d ?(z=0.,0.) b =
    let bmin, bmax = B2.min b, B2.max b in
    let z1, z2 = z in
      if z1 < z2 then
	(Point.of_2d ~z:z1 bmin, Point.of_2d ~z:z2 bmax)
      else
	(Point.of_2d ~z:z2 bmin, Point.of_2d ~z:z1 bmax)

  let clamp_point (bmin, bmax) p =
    V.min bmax (V.max bmin p)

  let clamp b (cmin, cmax) =
    clamp_point b cmin, clamp_point b cmax
  
  let union (bmin, bmax) (cmin, cmax) =
    V.min bmin cmin, V.max bmax cmax

  let intersection b c =
    clamp c (clamp b c)

  let min (bmin, _) = bmin
  let max (_, bmax) = bmax

  let x (bmin,bmax) = (V.x bmin, V.x bmax)
  let y (bmin,bmax) = (V.y bmin, V.y bmax)
  let z (bmin,bmax) = (V.z bmin, V.z bmax)
  
  let center (m1, m2) = V.mul1 (V.add m1 m2) 0.5
  let size (m1, m2) = V.sub m2 m1

  let classify_point (bmin, bmax) (x, y, z) =
    let x1, y1, z1 = bmin in
    let x2, y2, z2 = bmax in
    let xc = interval_classify ~eps x (x1, x2) in
    let yc = interval_classify ~eps y (y1, y2) in
    let zc = interval_classify ~eps z (z1, z2) in
      match xc, yc, zc with
	| Positive, _, _ -> Positive
	| _, Positive, _ -> Positive
	| _, _, Positive -> Positive
	| Epsilon, _, _ -> Epsilon
	| _, Epsilon, _ -> Epsilon
	| _, _, Epsilon -> Epsilon
	| Negative, Negative, Negative -> Negative

  let contains_point b p =
    classify_point b p <> Positive

  let contains b (cmin, cmax) =
    contains_point b cmin && contains_point b cmax

  let to_string (bmin, bmax) =
    Printf.sprintf
      ("AABB[min=%s, max= %s]")
      (V.to_string bmin)
      (V.to_string bmax)

  let intersects b c =
       interval_intersects ~eps (x b) (x c)
    && interval_intersects ~eps (y b) (y c)
    && interval_intersects ~eps (z b) (z c)

end

(*----------------------------------------------------------------------------*)

module Polyhedron =
struct
  type t = Halfspace.t list

  let make hs = hs

  let make_box (bmin, bmax) =
    [
      Halfspace.make_anchored bmin ~n:(V.neg V.ex);
      Halfspace.make_anchored bmin ~n:(V.neg V.ey);
      Halfspace.make_anchored bmin ~n:(V.neg V.ez);
      Halfspace.make_anchored bmax ~n:V.ex;
      Halfspace.make_anchored bmax ~n:V.ey;
      Halfspace.make_anchored bmax ~n:V.ez;
    ]

  let halfspaces ph = ph

  let transform at =
    List.map (Halfspace.transform at)
  
  let to_string ph =
    Printf.sprintf "Polyhedron[#hs=%d]" (List.length ph)
end

(*----------------------------------------------------------------------------*)

module Frame =
struct
  type t = T.t

  let make ?(origin=V.zero) ?(x=V.ex) ?(y=V.ey) ?(z=V.ez) () =
    let linear = M.of_cols x y z in
    let translation = origin in
      T.lmake ~linear ~translation ()

  let identity = T.identity
  let is_identity = T.is_identity

  let of_transform m = m
  let to_transform f = f

  let to_gl f = T.to_gl f

  let origin f = T.translation f

  let x f = M.col1 (T.linear f)

  let y f = M.col2 (T.linear f)

  let z f = M.col3 (T.linear f)

  let transform m f = T.concat m f
  let transform_locally m f = T.concat f m

  let move_to p f = T.make (T.linear f) p

  let look_at p f =
    let origin = origin f in
    let x = V.sub p origin in
      if (V.norm2 x) > FNum.delta then
	let x = V.normalize x in
	let y = V.normalize (~-.(V.y x), (V.x x), 0.) in
	let z = V.normalize (V.cross x y) in
	  make ~origin ~x ~y ~z ()
      else
	f

  let translate = T.translatev
  let rotate = T.rotatev

  let map_point = T.transform_point
  let map_vector = T.transform_vector
  let map_normal = T.transform_normal
  let inverse_map_point = T.inverse_transform_point
  let inverse_map_vector = T.inverse_transform_vector
  let inverse_map_normal = T.inverse_transform_normal

  let look_at_local p f =
    let p = map_point f p in
      look_at p f

  let map_transform f t =
    T.concat f (T.concat t (T.invert f))

  let to_string f =
    Printf.sprintf "Frame[%s + x*%s + y*%s + z*%s]"
      (Point.to_string (origin f))
      (V.to_string (x f))
      (V.to_string (y f))
      (V.to_string (z f))
end

(*----------------------------------------------------------------------------*)
