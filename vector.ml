(*----------------------------------------------------------------------------*)

(* TODO : assertions for mutating functions to prevent mangling of objects 
   which are input as well as output arguments.
   e.g.: M.mulm', M.invert' *)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

open Core

type ('a,'n) t = float array

type cswz1 = [`x|`nx]
type cswz2 = [`y|`ny|cswz1]
type cswz3 = [`z|`nz|cswz2]
type cswz4 = [`w|`nw|cswz3]

(*----------------------------------------------------------------------------*)

let make1 x = [|x|]
let make2 x y = [|x;y|]
let make3 x y z = [|x;y;z|]
let make4 x y z w= [|x;y;z;w|]

let dim a =
  Array.length a

let dimension a =
  N.__num_of_int__ (dim a)

let make ?(init=0.) d =
  Array.make (N.int d) init

let copied v =
  Array.copy v

let zeroed v =
  make (dimension v)

let fill' x dst =
  let n = dim dst in
    Array.fill dst 0 n x

let copy' from dst =
  Array.blit from 0 dst 0 (dim dst)

let xfer' v dst =
  let n = dim dst
  and m = dim v
  in
    Array.blit v 0 dst 0 (min n m)

let copy_to_array' v arr =
  let n = dim v in
    assert (Array.length arr == n);
    Array.blit v 0 arr 0 n

let copy_from_array' v arr =
  let n = dim v in
    assert (Array.length arr = n);
    Array.blit arr 0 v 0 n


let zero d =
  make ~init:0. d

let zero' dst =
  fill' 0. dst

let one d =
  make ~init:1. d

let one' dst =
  fill' 1. dst

(*------------------------------------*)

let of_array d a =
  assert (N.int d = dim a);
  a

let to_array a =
  a

let __absolve__ a =
  a

let __magic__ a =
  a

(*------------------------------------*)

(* no assert necessary here because ocaml checks indices *)
let get v i = v.(i)
let set' v i f = v.(i) <- f

let x v = v.(0)
let y v = v.(1)
let z v = v.(2)
let w v = v.(3)

let set_x' v f = v.(0) <- f
let set_y' v f = v.(1) <- f
let set_z' v f = v.(2) <- f 
let set_w' v f = v.(3) <- f

(*------------------------------------*)

let ex n =
  let v = zero n in
    set_x' v 1.;
    v

let ey n =
  let v = zero n in
    set_y' v 1.;
    v

let ez n = 
  let v = zero n in
    set_z' v 1.;
    v

let ew n =
  let v = zero n in
    set_w' v 1.;
    v

let ex' dst =
  let n = dim dst in
    dst.(0) <- 1.;
    for i = 1 to n-1 do
      dst.(i) <- 0.
    done
      
let ey' dst = 
  let n = dim dst in
    dst.(0) <- 0.;
    dst.(1) <- 1.;
    for i = 2 to n-1 do
      dst.(i) <- 0.
    done

let ez' dst = 
  let n = dim dst in
    dst.(0) <- 0.;
    dst.(1) <- 0.;
    dst.(2) <- 1.;
    for i = 3 to n-1 do
      dst.(i) <- 0.
    done

let ew' dst =
  let n = dim dst in
    dst.(0) <- 0.;
    dst.(1) <- 0.;
    dst.(2) <- 0.;
    dst.(3) <- 1.;
    for i = 4 to n-1 do
      dst.(i) <- 0.
    done

(*------------------------------------*)

let dot a b =
  match dim a with
      0 -> 0.
    | 1 -> a.(0)*.b.(0)
    | 2 -> a.(0)*.b.(0) +.a.(1)*.b.(1)
    | 3 -> a.(0)*.b.(0) +.a.(1)*.b.(1) +.a.(2)*.b.(2)
    | 4 -> a.(0)*.b.(0) +.a.(1)*.b.(1) +.a.(2)*.b.(2) +.a.(3)*.b.(3)
    | n ->
	let r = [|0.|] in
	  for i = 0 to n-1 do
	    r.(0) <- r.(0) +. a.(i)*.b.(i)
	  done;
	  r.(0)
	    
let magnitude_squared a =
  dot a a

let magnitude a =
  sqrt (magnitude_squared a)

(*------------------------------------*)

let ref_trans1 op1' v = 
  let c = zeroed v in
    op1' v c;
    c

let ref_trans2 op2' a b =
  let c = zeroed a in
    op2' a b c;
    c

(*------------------------------------*)

let neg' a dst =
  let n = dim dst in
    for i = 0 to n-1 do
      dst.(i) <- ~-. (a.(i))
    done

let sub' a b dst =
  let n = dim dst in
    for i = 0 to n-1 do
      dst.(i) <- a.(i) -. b.(i)
    done

let add' a b dst =
  let n = dim dst in
    for i = 0 to n-1 do
      dst.(i) <- a.(i) +. b.(i)
    done

let mul1' a s dst =
  let n = dim a in
    for i = 0 to n-1 do
      dst.(i) <- s*.a.(i)
    done

let mul1add' a s dst =
  let n = dim a in
    assert (n > 0);
    dst.(0) <- s*.a.(0);
    for i = 1 to n-1 do
      dst.(i) <- dst.(i) +. s*.a.(i)
    done

let mulvadd' a v dst =
  let n = dim a in
    assert (n > 0);
    dst.(0) <- a.(0)*.v.(0);
    for i = 1 to n-1 do
      dst.(i) <- dst.(i) +. a.(i)*.v.(i)
    done

let mulv' a b dst =
  let n = dim a in
    for i = 0 to n-1 do
      dst.(i) <- a.(i)*.b.(i)
    done

let min' a b dst =
  let n = dim a in
    for i = 0 to n-1 do
      let ai = a.(i)
      and bi = b.(i)
      in
	if ai < bi then
	  dst.(i) <- ai
	else
	  dst.(i) <- bi
    done

let max' a b dst =
  let n = dim a in
    for i = 0 to n-1 do
      let ai = a.(i)
      and bi = b.(i)
      in
	if ai > bi then
	  dst.(i) <- ai
	else
	  dst.(i) <- bi
    done

let neg  v   = ref_trans1 neg'  v
let sub  a b = ref_trans2 sub'  a b
let add  a b = ref_trans2 add'  a b
let mul1 a s = ref_trans2 mul1' a s
let mulv a v = ref_trans2 mulv' a v
let min  a b = ref_trans2 min'  a b
let max  a b = ref_trans2 max'  a b

let mul1add a s = ref_trans2 mul1add' a s
let mulvadd a v = ref_trans2 mulvadd' a v

(*------------------------------------*)

let cross' a b dst =
  (* assert (dst != a && dst != b); *)
  dst.(0) <- a.(1)*.b.(2) -. a.(2)*.b.(1);
  dst.(1) <- a.(2)*.b.(0) -. a.(0)*.b.(2);
  dst.(2) <- a.(0)*.b.(1) -. a.(1)*.b.(0)

let cross a b =
  let x = a.(1)*.b.(2) -. a.(2)*.b.(1) and
      y = a.(2)*.b.(0) -. a.(0)*.b.(2) and
      z = a.(0)*.b.(1) -. a.(1)*.b.(0)
  in
    make3 x y z

let invert' a dst =
  let invn2 = 1./.magnitude_squared a in
    mul1' a invn2 dst

let inverted a =
  let invn2 = 1./.magnitude_squared a in
    mul1 a invn2

let normalize' a dst =
  let invn = 1./.magnitude a in
    mul1' a invn dst

let normalized a =
  let invn = 1./.magnitude a in
    mul1 a invn

let mid' a b dst =
  let n = dim dst in
    for i = 0 to n-1 do
      dst.(i) <- 0.5 *. (a.(i) +. b.(i))
    done

let lerp' ~t a b dst =
  let n = dim dst in
  let omt = 1. -. t in
    for i = 0 to n-1 do
      dst.(i) <- omt*.a.(i) +. t*.b.(i)
    done

let minmax' a b dst1 dst2 =
  let n = dim a in
    for i = 0 to n-1 do
      let ai = a.(i) and bi = b.(i) in
	if ai > bi then begin
	  dst1.(i) <- bi;
	  dst2.(i) <- ai
	end else begin
	  dst1.(i) <- ai;
	  dst2.(i) <- bi
	end
    done

let swz' v i c dst =
  match c with
    | `x -> dst.(i) <- v.(0)
    | `y -> dst.(i) <- v.(1)
    | `z -> dst.(i) <- v.(2)
    | `w -> dst.(i) <- v.(3)
    | `nx -> dst.(i) <- ~-.(v.(0))
    | `ny -> dst.(i) <- ~-.(v.(1))
    | `nz -> dst.(i) <- ~-.(v.(2))
    | `nw -> dst.(i) <- ~-.(v.(3))

let swizzle2' c0 c1 v dst =
  assert(v != dst);
  swz' v 0 c0 dst;
  swz' v 1 c1 dst
    
let swizzle3' c0 c1 c2 v dst =
  assert(v != dst);
  swz' v 0 c0 dst;
  swz' v 1 c1 dst;
  swz' v 2 c2 dst

let swizzle4' c0 c1 c2 c3 v dst =
  assert(v != dst);
  swz' v 0 c0 dst;
  swz' v 1 c1 dst;
  swz' v 2 c2 dst;
  swz' v 3 c3 dst

(*------------------------------------*)

let mid a b = ref_trans2 mid' a b

let lerped ~t a b = ref_trans2 (lerp' ~t) a b

let minmax a b =
  let c = copied a and d = copied b in
    minmax' a b c d;
    (c, d)

let swizzled2 c0 c1 v =
  let c = copied v in
    swizzle2' c0 c1 v c;
    c

let swizzled3 c0 c1 c2 v =
  let c = copied v in
    swizzle3' c0 c1 c2 v c;
    c

let swizzled4 c0 c1 c2 c3 v =
  let c = copied v in
    swizzle4' c0 c1 c2 c3 v c;
    c

(*------------------------------------*)

let map f a =
  Array.map f a

let fold f a =
  Array.fold_right f a

let fold2 f a b z =
  let rec iter i s =
    match i with
      | 0 -> s
      | i ->
	  let i = i-1 in
	    iter i (f a.(i) b.(i) s)
  in
    iter (dim a) z

(*------------------------------------*)

open Format
open Hopp

let print fmt v =
  let n = dim v in
    pp_open_lbox fmt "[|";
    for i = 0 to n-1 do
      if i > 0 then begin
	pp_print_string fmt ";";
	pp_print_cut fmt ()
      end;
      fprintf fmt "%12.6g" v.(i)
    done;
    pp_close_lbox fmt "|]"

let to_string v = pp_make_to_string print v

(*------------------------------------*)

let is_equal ~eps a b =
  if a == b then true else
    try
      let n = dim a in
	for i = 0 to n-1 do 
	  if not (Flt.is_equal eps a.(i) b.(i)) then
	    raise Break
	done;
	true
    with
	Break -> false

let is_zero ~eps a =
  try
    let n = dim a in
      for i = 0 to n-1 do
	if not (Flt.is_equal eps a.(i) 0.) then
	  raise Break
      done;
      true
  with
      Break -> false

let is_uniform a x =
  try
    let n = dim a in
      for i = 0 to n-1 do
	if not (a.(i) = x) then
	  raise Break
      done;
      true
  with
      Break -> false

let random_unit ?(state=Rnd.default_state) d =
  let v = make d in
  let rec search () =
    let n = dim v in
      for i = 0 to n-1 do
	set' v i (1. -. Rnd.float state 2.)
      done;
      let m = magnitude v in
	if m > Flt.epsilon then begin
	  mul1' v (1./.m) v;
	  v
	end else
	  search ()
  in
    search ()

let random ?(state=Rnd.default_state) ?lower ?upper d =
  let lower = Option.may_default zero d lower in
  let upper = Option.may_default one d upper in
  let v = make d in
  let n = N.int d in
    for i = 0 to n-1 do
      let l = lower.(i) in
      let u = upper.(i) in
	v.(i) <- Rnd.float state (u -. l) +. l
    done;
    v

let random_gaussian ?(state=Rnd.default_state) ?mu ?sigma d =
  let mu = Option.may_default zero d mu in
  let sigma = Option.may_default one d sigma in
  let v = make d in
  let n = N.int d in
  let rec loop i =
    let g1, g2 = Rnd.box_mueller state in
      if i < n then begin
	let mu = mu.(i) in
	let sigma = sigma.(i) in
	  v.(i) <-  g1 *. sigma +. mu
      end;
      let i = i + 1 in
	if i < n then begin
	  let mu = mu.(i)  in
	  let sigma = sigma.(i) in
	    v.(i) <- g2 *. sigma +. mu
	end;
	let i = i + 1 in
	  if i < n then
	    loop i
	  else
	    v
  in
    loop 0

(*----------------------------------------------------------------------------*)

open OUnit
open Printf

let test_equal msg u v =
  assert_equal
    ~msg
    ~cmp:(is_equal ~eps:Flt.delta)
    ~printer:to_string
    u v

let test_vector_d d =
  (sprintf "D%d" (N.int d)) >::: [
    "one, inversed" >:: begin fun _ ->
      let one = one d in
	test_equal
	  "(inverted (inverted one)) = one"
	  (inverted (inverted one)) one
    end
  ]

let test_vector_2 =
  "D2spec" >::: [
    "swizzle2'" >:: begin fun _ ->
      let v = random_unit N._2 in
      let u =
	swizzled2 `ny`x 
	  (swizzled2 `y`nx v)
      in
	test_equal
	  "swizzled^2 == id"
	  u v
    end
  ]

let test_vector_3 =
  "D3spec" >::: [
    "swizzle3'" >:: begin fun _ ->
      let v = random_unit N._3 in
      let u =
	swizzled3 `nx`ny`nz
	  (swizzled3 `z`x`ny
	     (swizzled3 `nz`x`y
		(swizzled3 `z`nx`y v)))
      in
	test_equal
	  "swizzled^3 == id"
	  u v
    end
  ]

let test_vector_4 =
  "D4spec" >::: [
    "swizzle4'" >:: begin fun _ ->
      let v = random_unit N._4 in
      let u =
	swizzled4 `nx`ny`nz`nw
	  (swizzled4 `z`w`x`y 
	     (swizzled4 `nw`nz`ny`nx 
		(swizzled4 `y`x`w`z v)))
      in
	test_equal
	  "swizzled^4 == id"
	  u v
    end
  ]

let unit_test =
  "Vector" >::: [
    test_vector_d N._1;
    test_vector_d N._2;
    test_vector_d N._3;
    test_vector_d N._4;
    test_vector_2;
    test_vector_3;
    test_vector_4;
    "TODO" >:: fun _ -> todo "More assertions!";
  ]

(*----------------------------------------------------------------------------*)
