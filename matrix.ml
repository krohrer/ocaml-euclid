(*----------------------------------------------------------------------------*)

module IV = ImpVec
module IM = ImpMat
module V = Vector

type 'n dim = 'n N.t
type ('m,'n) t = { m : int; n : int; repr : IM.t }
type 'n vec = 'n V.t

exception NotInvertible

(*----------------------------------------------------------------------------*)

let init dm dn f =
  let m = N.int dm and n = N.int dn in
  let dst = IM.alloc ~m ~n in
  IM.init ~m ~n ~dst f;
  { m; n; repr = dst }

let make_cols vs dn =
  let n = N.int dn in
  assert (0 < n);
  assert (Array.length vs = n);
  let m = V.size vs.(0) in
  let dst = IM.alloc ~m ~n in
  for i = 0 to n-1 do
    IM.set_col ~m ~n ~dst i (V.__repr__ vs.(i))
  done;
  { m; n; repr = dst }

let make_rows dm vs =
  let m = N.int dm in
  assert (0 < m);
  assert (Array.length vs = m);
  let n = V.size vs.(0) in
  let dst = IM.alloc ~m ~n in
  for i = 0 to m-1 do
    IM.set_row ~m ~n ~dst i (V.__repr__ vs.(i))
  done;
  { m; n; repr = dst }

let zero dm =
  let dn = dm in
  let m = N.int dm and n = N.int dn in
  let dst = IM.alloc ~m ~n in
  IM.fill ~m ~n ~dst 0.0;
  { m; n; repr = dst }

let identity dm =
  let dn = dm in
  let m = N.int dm and n = N.int dn in
  let dst = IM.alloc ~m ~n in
  IM.eye ~m ~n ~dst;
  { m; n; repr = dst }

(*----------------------------------------------------------------------------*)

let __make__ dm dn repr =
  { m = N.int dm; n = N.int dn; repr }
  
let __repr__ { repr; _ } = repr

(*----------------------------------------------------------------------------*)

let to_array maj { m; n; repr } =
  match maj with
  | `column_major ->
    Array.copy repr
  | `row_major ->
    let dst = ImpMat.alloc ~m ~n in
    ImpMat.transpose ~m ~n ~dst repr;
    dst

let from_array maj dm dn arr =
  let m = N.int dm and n = N.int dn in
  assert (m*n = Array.length arr);
  match maj with
  | `column_major ->
    { m; n; repr = Array.copy arr }
  | `row_major ->
    let dst = ImpMat.alloc ~m ~n in
    ImpMat.transpose ~dst ~n:m ~m:n arr;
    { m; n; repr = dst }

(*----------------------------------------------------------------------------*)

let random state dm dn =
  init dm dn (fun i k -> Rnd.gaussian state ())

let random_invertible state dm =
  let dn = dm in
  let rec search () =
    try
      random state dm dn
    with
    | NotInvertible -> search ()
  in
  search ()

(*----------------------------------------------------------------------------*)

type 'a idx = int

let x = 0
and y = 1
and z = 2
and w = 3

let rows { m; _ } = m
let cols { n; _ } = n
let row_dim { m; _ } = N.__num_of_int__ m
let col_dim { n; _ } = N.__num_of_int__ n

let get { m; n; repr } ri ci = 
  IM.get ~m ~n repr ri ci

let row ({ m; n; repr } as a) ri =
  let dst = IV.alloc ~n in
  for ci = 0 to n-1 do
    dst.(ci) <- IM.get ~m ~n repr ri ci
  done;
  V.__make__ (row_dim a) dst

let col ({ m; n; repr } as a) ci =
  let dst = IV.alloc ~n:m in
  for ri = 0 to n-1 do
    dst.(ri) <- IM.get ~m ~n repr ri ci
  done;
  V.__make__ (col_dim a) dst

let row' = row
let col' = col
let get' = get

let diagonal ({ m; n; repr } as a) =
  let dst = IV.alloc ~n in
  for i = 0 to n-1 do
    dst.(i) <- IM.get ~m ~n repr i i
  done;
  V.__make__ (row_dim a) dst

(*----------------------------------------------------------------------------*)

let to_cols a =
  Array.init a.n (col a)

let to_rows a = 
  Array.init a.m (row a)

(*----------------------------------------------------------------------------*)

let neg { m; n; repr } =
  let dst = IM.alloc ~m ~n in
  IM.neg ~m ~n ~dst repr;
  { m; n; repr = dst }

let add { m; n; repr=r1 } { repr=r2; _ } =
  let dst = IM.alloc ~m ~n in
  IM.add ~m ~n ~dst r1 r2;
  { m; n; repr = dst }

let sub { m; n; repr=r1 } { repr=r2; _ } =
  let dst = IM.alloc ~m ~n in
  IM.sub ~m ~n ~dst r1 r2;
  { m; n; repr = dst }

let mul { m; n; repr=r1 } { n=o; repr=r2; _ } =
  let dst = IM.alloc ~m ~n:o in
  IM.mul ~dst ~m ~n ~o r1 r2;
  { m; n = o; repr = dst }
    
let smul s { m; n; repr } =
  let dst = IM.alloc ~m ~n in
  IM.smul ~dst ~m ~n s repr;
  { m; n; repr = dst }

let transpose { m; n; repr } =
  let dst = IM.alloc ~m:n ~n:m in
  IM.transpose ~dst ~m ~n repr;
  { m = n; n = m; repr = dst }

let invert { m; n; repr } =
  let dst = IM.alloc ~m ~n in
  IM.invert ~m ~dst repr;
  { m; n; repr = dst }

let ( ~- ) = neg
let ( +  ) = add
let ( -  ) = sub
let ( *  ) = mul
let ( *. ) = smul

let is_zero eps { m; n; repr } =
  ImpMat.is_zero eps ~m ~n repr

let is_equal eps { m; n; repr=r1 } { repr=r2; _ } =
  ImpMat.is_equal eps ~m ~n r1 r2

let is_identity eps { m; n; repr } =
  ImpMat.is_identity eps ~m ~n repr

(*----------------------------------------------------------------------------*)

(* open Format *)
(* open Hopp *)

(* let print fmt a = *)
(*   let m = rows a in *)
(*     pp_open_lvbox fmt "[|"; *)
(*     for i = 0 to m-1 do *)
(*       if i > 0 then begin *)
(* 	pp_print_string fmt ";"; *)
(* 	pp_print_cut fmt (); *)
(*       end; *)
(*       V.print fmt (row a i); *)
(*     done; *)
(*     pp_close_lbox fmt "|]" *)

(* let to_string a = pp_make_to_string print a *)

(*----------------------------------------------------------------------------*)

(* open OUnit *)
(* open Printf *)

(* let test_equal msg a b = *)
(*   let cmp c d = is_equal ~eps:Scalar.delta c d in *)
(*   let printer c = to_string c in *)
(*     assert_equal ~msg ~cmp ~printer a b *)

(* let test_matrix_d d = *)
(*   (sprintf "D%d" (N.int d)) >::: [ *)
(*     "identity, inversed" >:: begin fun _ -> *)
(*       let id = identity d in *)
(* 	test_equal *)
(* 	  "(inverted id) id" *)
(* 	  (inverted id) id *)
(*     end; *)
(*     "transpose, get_diagnoal, diagonal" >:: begin fun _ -> *)
(*       () *)
(*     end; *)
(*     "set_diagonal', get_diagonal, diagonal" >:: begin fun _ -> *)
(*       let m = random_invertible d in *)
(*       let v = V.random d in *)
(*       let u = V.make d in *)
(* 	set_diagonal' m v; *)
(* 	get_diagonal' m u; *)
(* 	V.test_equal *)
(* 	  "set_diagonal'/get_diagonal'" *)
(* 	  u v *)
(*     end; *)
(*     "mulm, identity" >:: begin fun _ -> *)
(*       let m = random_invertible d in *)
(*       let n = identity d in *)
(* 	test_equal *)
(* 	  "multiplicative identity" *)
(* 	  m (mulm m n) *)
(*     end; *)
(*     "mulm, inversed" >:: begin fun _ -> *)
(*       let m = random_invertible d in *)
(*       let n = identity d in  *)
(* 	test_equal *)
(* 	  "inverse" *)
(* 	  n (mulm (inverted m) m) *)
(*     end; *)
(*     "inverse, vmul" >:: begin fun _ -> *)
(*       let m = random_invertible d in *)
(*       let m' = copied m in *)
(*       let r = V.random d in *)
(* 	invert' m m'; *)
(* 	V.test_equal *)
(* 	  "vmul (vmul r m) (inv m) = r" *)
(* 	  r (vmul (vmul r m) m') *)
(*     end; *)
(*     "inverse, mulv" >:: begin fun _ -> *)
(*       let m = random_invertible d in *)
(*       let m' = copied m in *)
(*       let r = V.random d in *)
(* 	invert' m m'; *)
(* 	V.test_equal *)
(* 	  "mulv (inv m) (mulv m r) = r" *)
(* 	  r (mulv m' (mulv m r)) *)
(*     end *)
(*   ] *)

(* let unit_test = *)
(*   "Matrix" >::: [ *)
(*     test_matrix_d N._1; *)
(*     test_matrix_d N._2; *)
(*     test_matrix_d N._3; *)
(*     test_matrix_d N._4; *)
(*     "TODO" >:: fun _ -> todo "More assertions!"; *)
(*   ] *)

(*----------------------------------------------------------------------------*)
