(* Kaspar Rohrer, Mon Jan 19 21:59:38 CET 2009 *)

(*----------------------------------------------------------------------------*)

(* TODO : assertions for mutating functions to prevent mangling of objects 
   which are input as well as output arguments.
   e.g.: M.mulm', M.invert' *)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

open Core
module V = Vector

type ('a,'m,'n) t = float array array

(*----------------------------------------------------------------------------*)

type majority = [`row_major | `column_major]

let rows r =
  Array.length r

let columns r =
  let c = r.(0) in
  Array.length c

let row_dimension a =
  N.__num_of_int__ (rows a)

let column_dimension a =
  N.__num_of_int__ (columns a)

let make_matrix m n init =
  Array.make_matrix m n init

let make ?(init=0.) dm dn =
  make_matrix (N.int dm) (N.int dn) init

let copied a =
  let c = Array.copy a
  and n = rows a
  in
    for i = 0 to n-1 do
      c.(i) <- Array.copy c.(i)
    done;
    c

let get_minor' a ri ci dst =
  let m = rows dst in
  let n = columns dst in
    assert (ri <= m && ci <= n);
    for i = 0 to m-1 do
      let dst_i = dst.(i)
      and   a_i = if i >= ri then a.(i+1) else a.(i)
      in
	Array.blit a_i 0 dst_i 0 ci;
	Array.blit a_i (ci+1) dst_i ci (n-ci)
    done

let fill' x dst =
  let m = rows dst in
  let n = columns dst in
    for i = 0 to m-1 do
      Array.fill dst.(i) 0 n x
    done

let copy' a dst =
  let m = rows dst in
  let n = columns dst in
    for i = 0 to m-1 do
      Array.blit a.(i) 0 dst.(i) 0 n
    done

let xfer' a dst =
  let m = min (rows dst) (rows a) in
  let n = min (columns dst) (columns dst) in
    for i = 0 to m-1 do
      Array.blit a.(i) 0 dst.(i) 0 n
    done

(*------------------------------------*)

let ref_trans1 op1' v = 
  let c = make (row_dimension v) (column_dimension v) in
    op1' v c;
    c

let ref_trans2 op2' a b =
  let c = make (row_dimension a) (column_dimension a) in
    op2' a b c;
    c

(*------------------------------------*)

let transpose' a dst =
  assert (dst != a);
  let m = rows a in
  let n = columns a in
    for i = 0 to n-1 do
      let dst_i = dst.(i) in
	for k = 0 to m-1 do
	  dst_i.(k) <- a.(k).(i)
	done
    done

let transpose_inplace' a =
  let n = rows a in
    for i = 0 to n-2 do
      let a_i = a.(i) in
	for k = 1 to n-1 do
	  let a_k = a.(k) in
	  let t = a_i.(k) in
	    a_i.(k) <- a_k.(i);
	    a_k.(i) <- t
	done
    done
      
(*------------------------------------*)

let of_arrays dm dn a =
  assert (N.int dm = rows a);
  assert (N.int dn = columns a);
  a

let to_arrays a =
  a

let of_array maj dm dn a1d =
  let a = make dm dn in
  let m = N.int dm in
  let n = N.int dn in
    match maj with
	`row_major ->
	  for i = 0 to m-1 do
	    let a_i = a.(i) and 
		i_n = i*n in
	      for k = 0 to n-1 do
		a_i.(k) <- a1d.(i_n + k)
	      done
	  done;
	  a
      | `column_major ->
	  for i = 0 to m-1 do
	    let a_i = a.(i) in
	      for k = 0 to n-1 do
		a_i.(k) <- a1d.(i + k*m)
	      done
	  done;
	  a

let to_array maj a =
  let m = rows a in
  let n = columns a in
  let a1d = Array.make (m*n) 0. in
    match maj with
	`row_major ->
	  for i = 0 to m-1 do
	    let a_i = a.(i) and
		i_n = i*n in
	      for k = 0 to n-1 do
		a1d.(i_n + k) <- a_i.(k)
	      done
	  done;
	  a1d
      | `column_major ->
	  for i = 0 to m-1 do
	    let a_i = a.(i) in
	      for k = 0 to n-1 do
		a1d.(i + k*m) <- a_i.(k)
	      done
	  done;
	  a1d

let __absolve__ a = a
let __magic__ a = a

(*------------------------------------*)

(* programmers notation (from 0 to n-1) (row first, column second) *)

(* internal representation is row-major *)

let get a i k = a.(i).(k)
let set' a i k x = a.(i).(k) <- x

let get_row' a i dst =
  let n = columns a in
  let a_i = a.(i) in
    for k = 0 to n-1 do
      V.set' dst k a_i.(k)
    done

let get_column' a i dst =
  let m = rows a in
    for k = 0 to m-1 do
      V.set' dst k a.(k).(i)
    done

let get_diagonal' a dst =
  let n = rows a in
    for k = 0 to n-1 do
      V.set' dst k a.(k).(k) 
    done

let set_row' a i v =
  let n = columns a in
  let a_i = a.(i) in
    for k = 0 to n-1 do
      a_i.(k) <- V.get v k
    done

let set_column' a i v = 
  let m = rows a in
    for k = 0 to m-1 do
      a.(k).(i) <- V.get v k
    done

let set_diagonal' a v =
  let n = rows a in
    for k = 0 to n-1 do
      a.(k).(k) <- V.get v k
    done

let row a i =
  V.__absolve__ (V.of_array (column_dimension a) a.(i))

let column a i = 
  let c = V.make (row_dimension a) in
    get_column' a i c;
    V.__absolve__ c

let diagonal a =
  let c = V.make (row_dimension a) in
    get_diagonal' a c;
    V.__absolve__ c

(*------------------------------------*)

let neg' a dst =
  let m = rows dst in
  let n = columns dst in
    for i = 0 to m-1 do
      let dst_i = dst.(i) and a_i = a.(i) in
	for k = 0 to n-1 do
	  dst_i.(k) <- ~-.(a_i.(k))
	done
    done

let sub' a b dst =
  let m = rows dst in
  let n = columns dst in
    for i = 0 to m-1 do
      let dst_i = dst.(i) and a_i = a.(i) and b_i = b.(i) in
	for k = 0 to n-1 do
	  dst_i.(k) <- a_i.(k) -. b_i.(k)
	done
    done

let add' a b dst =
  let m = rows dst in
  let n = columns dst in
    for i = 0 to m-1 do
      let dst_i = dst.(i) and a_i = a.(i) and b_i = b.(i) in
	for k = 0 to n-1 do
	  dst_i.(k) <- a_i.(k) +. b_i.(k)
	done
    done

let mul1' a s dst =
  let m = rows dst in
  let n = columns dst in
    for i = 0 to m-1 do
      let dst_i = dst.(i) and a_i = a.(i) in
	for k = 0 to n-1 do
	  dst_i.(k) <- s*.a_i.(k)
	done
    done

(* internal representation is row-major *)
let mulm' a b dst =
  assert (dst != a && dst != b); (* may not share representation! *)
  let m = rows dst in
  let n = columns dst in
  let l = rows b in
    for i = 0 to m-1 do
      (* i >= 0 holds *)
      let dst_i = dst.(i) and a_i = a.(i) in
	for k = 0 to n-1 do
	  (* i >= 0 && k >= 0 holds *)
	  (* we dont need to initialize with 0., this is sufficient *)
	  dst_i.(k) <- a_i.(0)*.b.(0).(k);
	  (* just handle the first one separately (see loop invariants) *)
	  for ii = 1 to l-1 do
	    dst_i.(k) <- dst_i.(k) +. a_i.(ii)*.b.(ii).(k)
	  done
	done
    done

(* internal representation is row-major *)
let mulv' a v dst =
  let m = rows a in
  let n = columns a in
  let v = V.to_array v in
  let dst = V.to_array dst in
    assert (m > 0 && n > 0);
    let a_0 = a.(0) in
      for k = 0 to n-1 do
    	dst.(k) <- v.(0) *. a_0.(k)
      done;
      for i = 1 to m-1 do
    	let a_i = a.(i) in
    	  for k = 0 to n-1 do
    	    dst.(k) <- dst.(k) +. a_i.(k) *. v.(i)
    	  done
      done

(* internal representation is row-major *)
let vmul' v a dst =
  let dst' = V.to_array dst in
  let m = rows a in
  let n = columns a in
    assert(m > 0 && n > 0);
    begin
      let s = V.get v 0 and a_0 = a.(0) in
	for k = 0 to n-1 do
	  dst'.(k) <- s *. a_0.(k)
	done
    end;
    for i = 1 to m-1 do
      let s = V.get v i and a_i = a.(i) in
	for k = 0 to n-1 do
	  dst'.(k) <- dst'.(k) +. s *. a_i.(k)
	done
    done

(*------------------------------------*)

(* Helper function *)
let fi i = if (i land 1) = 0 then 1. else -1.

let rec det a =
  match rows a with
    | 0 -> 
	0.
    | 1 ->
	a.(0).(0)
    | 2 ->
	a.(0).(0)*.a.(1).(1) -. a.(1).(0)*.a.(0).(1)
    | n ->
	(* TODO : specialize for 3 and 4 dimensions *)
	let sum = [|0.|] in
	let m0i = make_matrix (n-1) (n-1) 0. in
	  for i = 0 to n-1 do
	    get_minor' a 0 i m0i;
	    sum.(0) <- sum.(0) +. a.(0).(i)*.(fi i)*.det m0i
	  done;
	  sum.(0)

(* Specialized function for 2x2 *)
let invert2' a dst =
  let a_0 = a.(0)
  and a_1 = a.(1)
  in
  let a00 = a_0.(0)
  and a01 = a_0.(1)
  and a10 = a_1.(0)
  and a11 = a_1.(1)
  in
  let d = a00*.a11 -. a10*.a01 in
  let p = 1./.d in
  let n = ~-.p in
  let dst_0 = dst.(0)
  and dst_1 = dst.(1)
  in
    dst_0.(0) <- p*.a11;
    dst_0.(1) <- n*.a01;
    dst_1.(0) <- n*.a10;
    dst_1.(1) <- p*.a00

let invert' a dst =
  assert (dst != a);
  match rows dst with
    | 0 ->
	()
    | 1 ->
	dst.(0).(0) <- 1./.a.(0).(0)
    | 2 ->
	invert2' a dst
    | n ->
	(* TODO : specialize for 3 and 4 dimensions *)
	let d = det a in
	let invd = 1./.d in
	let mik = make_matrix (n-1) (n-1) 0. in
	  for i = 0 to n-1 do
	    for k = 0 to n-1 do
	      get_minor' a i k mik;
	      dst.(k).(i) <- invd*.(fi (i+k))*.(det mik)
	    done;
	  done

(*------------------------------------*)

let neg  a   = ref_trans1 neg'  a
let sub  a b = ref_trans2 sub'  a b
let add  a b = ref_trans2 add'  a b
let mul1 a s = ref_trans2 mul1' a s
let mulm a b = ref_trans2 mulm' a b
let mulv a v = let c = V.make (row_dimension a) in mulv' a v c; V.__absolve__ c
let vmul v a = let c = V.make (column_dimension a) in vmul' v a c; V.__absolve__ c

let inverted   a = ref_trans1 invert'   a
let transposed a = ref_trans1 transpose' a

(*------------------------------------*)

let zero' dst =
  fill' 0. dst

let identity' dst =
  fill' 0. dst;
  let n = rows dst in
    for i = 0 to n-1 do
      dst.(i).(i) <- 1.
    done

let zero m n =
  make ~init:0. m n

let identity d  =
  let a = zero d d in
  let n = N.int d in
    for i = 0 to n-1 do
      a.(i).(i) <- 1.
    done;
    a

let identity2 = identity N._2
let identity3 = identity N._3
let identity4 = identity N._4

(*------------------------------------*)

let is_zero ~eps a =
  try
    let m = rows a in
    let n = columns a in
      for i = 0 to m-1 do
	let a_i = a.(i)
	in
	  for k = 0 to n-1 do
	    if not (Flt.is_zero ~eps a_i.(k)) then
	      raise Break
	  done
      done;
      true
  with
      Break -> false

let is_equal ~eps a b =
  if a == b then true else
    try
      let m = rows a in
      let n = columns a in
	for i = 0 to m-1 do 
	  let a_i = a.(i)
	  and b_i = b.(i)
	  in
	    for k = 0 to n-1 do
	      if not (Flt.is_equal eps a_i.(k) b_i.(k)) then
		raise Break
	    done
	done;
	true
    with
	Break -> false

let is_identity ~eps a =
  try
    let n = rows a in
      for i = 0 to n-1 do
	let a_i = a.(i) in
	  for k = 0 to n-1 do
	    let x = if k = i then 1. else 0. in
	      if not (Flt.is_equal eps a_i.(k) x) then
		raise Break
	  done
      done;
      true
  with
      Break -> false

(*------------------------------------*)

open Format
open Hopp

let print fmt a =
  let m = rows a in
    pp_open_lvbox fmt "[|";
    for i = 0 to m-1 do
      if i > 0 then begin
	pp_print_string fmt ";";
	pp_print_cut fmt ();
      end;
      V.print fmt (row a i);
    done;
    pp_close_lbox fmt "|]"

let to_string a = pp_make_to_string print a

let random ?(state=Rnd.default_state) dm dn =
  let a = make dm dn in
  let m = N.int dm in
  let n = N.int dn in
    for i = 0 to m-1 do
      for k = 0 to n-1 do
	set' a i k (1. -. Rnd.float state 2.)
      done
    done;
    a

let random_invertible ?(state=Rnd.default_state) dn =
  let a = make dn dn in
  let rec search () =
    let n = N.int dn in
      for i = 0 to n-1 do
	for k = 0 to n-1 do
	  set' a i k (1. -. Rnd.float state 2.)
	done
      done;
      if det a > Flt.delta then
	a
      else
	search ()
  in
    search ()

(*----------------------------------------------------------------------------*)

open OUnit
open Printf

let test_equal msg a b =
  let cmp c d = is_equal ~eps:Flt.delta c d in
  let printer c = to_string c in
    assert_equal ~msg ~cmp ~printer a b

let test_matrix_d d =
  (sprintf "D%d" (N.int d)) >::: [
    "identity, inversed" >:: begin fun _ ->
      let id = identity d in
	test_equal
	  "(inverted id) id"
	  (inverted id) id
    end;
    "transpose, get_diagnoal, diagonal" >:: begin fun _ ->
      ()
    end;
    "set_diagonal', get_diagonal, diagonal" >:: begin fun _ ->
      let m = random_invertible d in
      let v = V.random d in
      let u = V.make d in
	set_diagonal' m v;
	get_diagonal' m u;
	V.test_equal
	  "set_diagonal'/get_diagonal'"
	  u v
    end;
    "mulm, identity" >:: begin fun _ ->
      let m = random_invertible d in
      let n = identity d in
	test_equal
	  "multiplicative identity"
	  m (mulm m n)
    end;
    "mulm, inversed" >:: begin fun _ ->
      let m = random_invertible d in
      let n = identity d in 
	test_equal
	  "inverse"
	  n (mulm (inverted m) m)
    end;
    "inverse, vmul" >:: begin fun _ ->
      let m = random_invertible d in
      let m' = copied m in
      let r = V.random d in
	invert' m m';
	V.test_equal
	  "vmul (vmul r m) (inv m) = r"
	  r (vmul (vmul r m) m')
    end;
    "inverse, mulv" >:: begin fun _ ->
      let m = random_invertible d in
      let m' = copied m in
      let r = V.random d in
	invert' m m';
	V.test_equal
	  "mulv (inv m) (mulv m r) = r"
	  r (mulv m' (mulv m r))
    end
  ]

let unit_test =
  "Matrix" >::: [
    test_matrix_d N._1;
    test_matrix_d N._2;
    test_matrix_d N._3;
    test_matrix_d N._4;
    "TODO" >:: fun _ -> todo "More assertions!";
  ]

(*----------------------------------------------------------------------------*)
