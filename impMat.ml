exception NotInvertible

type t = float array
type vec = ImpVec.t

let length a = Array.length a

let alloc ~m ~n = ImpVec.alloc ~n:(m*n)

let chkdim ~m ~n a =
  length a = m*n

let get ~m ~n a i k =
  assert (chkdim ~m ~n a);
  a.(i*m + k)
  
let set ~m ~n ~dst i k s =
  assert (chkdim ~m ~n dst);
  dst.(i*m + k) <- s

let set_col ~m ~n ~dst ci v =
  for ri = 0 to m-1 do
    set ~m ~n ~dst ri ci v.(ri)
  done

let set_row ~m ~n ~dst ri v =
  for ci = 0 to n-1 do
    set ~m ~n ~dst ri ci v.(ci)
  done

let set_diag ~m ~dst v =
  let n = m in
  for i = 0 to m-1 do
    set ~m ~n ~dst i i v.(i)
  done

let init ~m ~n ~dst f =
  assert (chkdim ~m ~n dst);
  for i = 0 to m-1 do
    for k = 0 to n-1 do
      set ~m ~n ~dst i k (f i k)
    done
  done

let init ~m ~n ~dst f =
  for i = 0 to m-1 do
    for k = 0 to n-1 do
      set ~m ~n ~dst i k (f i k)
    done
  done

let fill ~m ~n ~dst s =
  for i = 0 to m*n-1 do
    dst.(i) <- s
  done

let eye ~m ~n ~dst =
  init ~m ~n ~dst (fun i k -> if i = k then 1. else 0.)

(*------------------------------------*)

let neg ~m ~n = ImpVec.neg ~n:(m*n)
let add ~m ~n = ImpVec.add ~n:(m*n)
let sub ~m ~n = ImpVec.sub ~n:(m*n)

let smul ~dst s ~m ~n a =
  assert (chkdim ~m ~n dst);
  for i = 0 to m*n-1 do
    dst.(i) <- s *. a.(i)
  done

(*      k  
    j   l
      n B
  i m A D

	A : m x n
	B : n x l
	D : m x l
*)
let mul ~dst ~m ~n a ~o b =
  let a i k = get ~m ~n a i k in
  let b i k = get ~m:n ~n:o b i k in
  let d i k = get ~m ~n:o dst i k in
  let setd i k s = set ~m ~n:o ~dst i k s in
  let addd i k s = setd i k (d i k +. s) in
  for i = 0 to m-1 do
    for k = 0 to o-1 do
      setd i k 0.;
      for j = 0 to n-1 do
	addd i k (a i j *. b j k)
      done
    done
  done

(*
      1
    n V
  m A D
*)
let mulv ~dst ~m ~n a v =
  mul ~dst ~m ~n a ~o:1 v

(*    n
    m A
  1 V D
*)
let tvmul ~dst v ~m ~n a = 
  mul ~dst ~m:1 ~n:m v ~o:n a

let tmulv ~dst ~m ~n a v =
  tvmul ~dst v ~m ~n a

let transpose ~m ~n ~dst a =
  assert (chkdim ~m ~n a);
  assert (chkdim ~m:n ~n:m dst);
  init ~m:n ~n:m ~dst (fun k i -> get ~m ~n a i k)

let dump ~m ~n a = 
  Printf.(
    printf "[\n";
    for i = 0 to n-1 do
      printf "\t";
      for k = 0 to m-1 do
	if 0 < k then print_string ", ";
	printf "% 10.f" (get ~m ~n a k i);
      done;
      printf "\n"
    done;
    printf "]\n"
  )

(*------------------------------------*)

let invert ~m ~dst a =
  let n = m in
  let im = 
    let make_row i =
      let r_i = ImpVec.alloc ~n:(2*n) in
      for k = 0 to 2*n - 1 do
	if k < n then
	  r_i.(k) <- get ~m ~n a i k
	else
	  r_i.(k) <- if k - n = i then 1. else 0.
      done;
      r_i
    in
    Array.init n make_row
  in
  let swap_rows i j =
    if i <> j then (
      let t = im.(i) in
      im.(i) <- im.(j);
      im.(j) <- t
    )
  in
  let find_pivot i =
    let score j = Float.abs im.(j).(i) in
    let rec fold j' s' = function
      | j when j < n ->
	let s = score i in
	if s' < s then
	  fold j s (j+1)
	else
	  fold j' s' (j+1)
      | _ -> j'
    in
    fold i (score i) (i+1)
  in
  (* find pivot row and swap to i-th position, then scale row so that
     element at (i,i) is 1.0 *)
  for i = 0 to n-1 do
    let pivot = find_pivot i in
    swap_rows pivot i;
    let inv_s = 1. /. im.(i).(i) in
    for k = i to 2*n-1 do
      im.(i).(k) <- inv_s *. im.(i).(k)
    done;
    (* Pre : im.(i).(i) = 1. *)
    for j = i+1 to n-1 do
      let s = im.(j).(i) in
      for k = i to 2*n-1 do
	im.(j).(k) <- im.(j).(k) -. s*.im.(i).(k)
      done (* Post : im.(j).(i) = 0.0 *)
    done (* Post : forall j, i < j: im.(j).(i) = 0.0 *)
  done (* Post : im in row echelon form *)
  ; (* substract multiple of i-th row from other rows (i-1 downto j) *)
  for i = n-1 downto 1 do
    for j = 0 to i-1 do
      (* we don't care about the left part as we already know it's
	 going to be the identity. The only important part is that we
	 are zeroing out the i-th column (except in the i-th row) by substracting *)
      let s = im.(j).(i) in
      for k = n to 2*n-1 do
	im.(j).(k) <- im.(j).(k) -. s*.im.(i).(k)
      done
    done
  done
  ; (* extract inverse *)
  for i = 0 to n-1 do
    for k = 0 to n-1 do
      let s = im.(i).(k+n) in
      if s = s then (* Check for NaN *)
	set ~m ~n ~dst i k s
      else
	(* Not invertible if NaN element exists *)
	raise NotInvertible
    done
  done
    
let is_equal eps ~m ~n a b =
  ImpVec.is_equal eps ~n:(m*n) a b

let is_zero eps ~m ~n a =
  ImpVec.is_zero eps ~n:(m*n) a

let is_identity eps ~m ~n a =
  m = n && (
    try
      for i = 0 to m-1 do
	for k = 0 to m-1 do
	  let e = get ~m ~n a i k in
	  let ok = Float.is_equal eps (if i = k then 1.0 else 0.0) e in
	  if not ok then raise Exit
	done
      done;
      true
    with Exit -> false
  )
