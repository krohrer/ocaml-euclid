(*----------------------------------------------------------------------------*)

open Core

type 'n t = float array

type swz1 = [`x|`nx]
type swz2 = [`y|`ny|swz1]
type swz3 = [`z|`nz|swz2]
type swz4 = [`w|`nw|swz3]

(*----------------------------------------------------------------------------*)

module Unsafe =
  struct
  end

(*----------------------------------------------------------------------------*)

let alloc n =
  alloc_float_array n

let make f d =
  let n = (N.int d) in
  let v = alloc n in
  for i = 0 to n-1 do v.(i) <- f i done;
  v

let make1 x = [|x|]
let make2 x y = [|x;y|]
let make3 x y z = [|x;y;z|]
let make4 x y z w= [|x;y;z;w|]

let size a =
  Array.length a

let dimension a =
  N.__num_of_int__ (size a)

(*------------------------------------*)

let memoize f =
  let s1 = f 1 and
      s2 = f 2 and
      s3 = f 3 and
      s4 = f 4 in
  fun d -> match N.int d with
  | 1 -> s1
  | 2 -> s2
  | 3 -> s3
  | 4 -> s4
  | n -> f n

let zeroN n = Array.make n 0.
let oneN n  = Array.make n 1.
let unityN k n =
  let v = alloc n in
  for i = 0 to n-1 do v.(i) <- if k = i then 1. else 0. done;
  v

let zero d = memoize zeroN d
let one d  = memoize oneN  d

let ex d = memoize (unityN 0) d
let ey d = memoize (unityN 1) d
let ez d = memoize (unityN 2) d
let ew d = memoize (unityN 3) d

(*------------------------------------*)

let to_array a = Array.copy a

let from_array a d =
  if N.int d == Array.length a then
    Array.copy a
  else
    invalid_arg "Vector.from_array: mismatched size."

external __repr__ : 'n t -> float array = "%identity"

(*------------------------------------*)

(* no assert necessary here because ocaml checks indices *)
let get i v = v.(i)

let x = get 0
let y = get 1
let z = get 2
let w = get 3

(*------------------------------------*)

let purify1 fmut a =
  let n = size a in
  let c = alloc n in
  fmut a c;
  c

let purify2 fmut a b =
  let n = size a in
  let c = alloc n in
  fmut a b c;
  c

let purify1n fmut a = purify1 (fmut (size a)) a
let purify2n fmut a b = purify2 (fmut (size a)) a b

(*------------------------------------*)

let neg' n a c =
  for i = 0 to n-1 do c.(i) <- (~-.) a.(i) done

let add' n a b c =
  for i = 0 to n-1 do c.(i) <- a.(i) +. b.(i) done

let sub' n a b c =
  for i = 0 to n-1 do c.(i) <- a.(i) -. b.(i) done

let mul' n a b c =
  for i = 0 to n-1 do c.(i) <- a.(i) *. b.(i) done

let smul' n s a c =
  for i = 0 to n-1 do c.(i) <- s *. a.(i) done

let div' n a b c =
  for i = 0 to n-1 do c.(i) <- a.(i) /. b.(i) done

let divs' n a s c =
  smul' n (1./.s) a c

let abs' n a c =
  for i = 0 to n-1 do c.(i) <- Float.abs a.(i) done

let min' n a b c =
  for i = 0 to n-1 do c.(i) <- Float.min a.(i) b.(i) done

let max' n a b c =
  for i = 0 to n-1 do c.(i) <- Float.max a.(i) b.(i) done

let neg = purify1n neg'
let add = purify2n add'
let sub = purify2n sub'
let mul = purify2n mul'
let div = purify2n div'
let smul s a = purify1 (smul' (size a) s) a
let divs = purify2n divs'
let abs = purify1n abs'
let min = purify2n min'
let max = purify2n max'

(*------------------------------------*)

let dot a b =
  match size a with
    0 -> 0.
  | 1 -> a.(0)*.b.(0)
  | 2 -> a.(0)*.b.(0) +.a.(1)*.b.(1)
  | 3 -> a.(0)*.b.(0) +.a.(1)*.b.(1) +.a.(2)*.b.(2)
  | 4 -> a.(0)*.b.(0) +.a.(1)*.b.(1) +.a.(2)*.b.(2) +.a.(3)*.b.(3)
  | n ->
    let rec iter i sum =
      if i < n then iter (i+1) (sum +. a.(i)*.b.(i)) else sum
    in
    iter 0 0.
      
let lensq a = dot a a
let len a = sqrt (lensq a)

(*------------------------------------*)

let cross' a b c =
  c.(0) <- a.(1)*.b.(2) -. a.(2)*.b.(1);
  c.(1) <- a.(2)*.b.(0) -. a.(0)*.b.(2);
  c.(2) <- a.(0)*.b.(1) -. a.(1)*.b.(0)

let normalize' n a c =
  divs' n a (len a) c

let mid' n a b c =
  for i = 0 to n-1 do
    c.(i) <- 0.5 *. (a.(i) +. b.(i))
  done

let lerp' n ~t a b c =
  let omt = 1. -. t in
  for i = 0 to n-1 do
    c.(i) <- omt*.a.(i) +. t*.b.(i)
  done

let minmax' n a b cmin cmax =
  for i = 0 to n-1 do
    let ai = a.(i) and bi = b.(i) in
    if ai > bi then begin
      cmin.(i) <- bi; cmax.(i) <- ai
    end else begin
      cmin.(i) <- ai; cmax.(i) <- bi
    end
  done

let cross = purify2 cross'
let normalize = purify1n normalize'
let mid = purify2n mid'

let lerp ~t a b =
  let n = size a in
  let c = alloc n in
  lerp' n ~t a b c;
  c

let minmax a b =
  let n = size a in
  let cmin = alloc n and cmax = alloc n in
  minmax' n a b cmin cmax;
  cmin, cmax

let swzsel a = function
  | `x -> a.(0)
  | `y -> a.(1)
  | `z -> a.(2)
  | `w -> a.(3)
  | `nx -> (~-.) a.(0)
  | `ny -> (~-.) a.(1)
  | `nz -> (~-.) a.(2)
  | `nw -> (~-.) a.(3)

let swizzle2' sx sy a c =
  c.(0) <- swzsel a sx;
  c.(1) <- swzsel a sy

let swizzle3' sx sy sz a c =
  c.(0) <- swzsel a sx;
  c.(1) <- swzsel a sy;
  c.(2) <- swzsel a sz

let swizzle4' sx sy sz sw a c =
  c.(0) <- swzsel a sx;
  c.(1) <- swzsel a sy;
  c.(2) <- swzsel a sz;
  c.(3) <- swzsel a sw

let swizzle2 sx sy a		= purify1 (swizzle2' sx sy) a
let swizzle3 sx sy sz a		= purify1 (swizzle3' sx sy sz) a
let swizzle4 sx sy sz sw a	= purify1 (swizzle4' sx sy sz sw) a

(*------------------------------------*)

open Format
open Hopp

let print fmt v =
  let n = size v in
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

let is_equal eps a b =
  if a == b then true else
    try
      let n = size a in
      for i = 0 to n-1 do 
	if not (Float.is_equal eps a.(i) b.(i)) then
	  raise Exit
      done;
      true
    with
      Exit -> false

let is_zero eps a =
  try
    let n = size a in
    for i = 0 to n-1 do
      if not (Float.is_equal eps a.(i) 0.) then
	raise Exit
    done;
    true
  with
    Exit -> false

let random_unit ?(state=Rnd.default) d =
  let n = N.int d in
  let c = alloc n in
  let rec search () =
    for i = 0 to n-1 do
      c.(i) <- 1. -. Rnd.float state 2.
    done;
    let lsq = lensq c in
    if Float.(is_zero delta lsq) && lsq < 1. then begin
      divs' n c (sqrt lsq) c;
      c
    end else
      search ()
  in
  search ()

let random ?(state=Rnd.default) ?lower ?upper d =
  let lower = Option.may_default zero d lower in
  let upper = Option.may_default one d upper in
  let n = N.int d in
  let c = alloc n in
  for i = 0 to n-1 do
    let l = lower.(i) in
    let u = upper.(i) in
    c.(i) <- Rnd.float state (u -. l) +. l
  done;
  c

let random_gaussian ?(state=Rnd.default) ?mu ?sigma d =
  let mu = Option.may_default zero d mu in
  let sigma = Option.may_default one d sigma in
  let n = N.int d in
  let c = alloc n in
  for i = 0 to n-1 do
    c.(i) <- Rnd.gaussian state ~mu:mu.(i) ~sigma:sigma.(i) ()
  done;
  c

(*----------------------------------------------------------------------------*)

let ( ~- ) = neg
let ( +  ) = add
let ( -  ) = sub
let ( *  ) = mul
let ( *. ) = smul
let ( /  ) = div
let ( /. ) = divs

(*----------------------------------------------------------------------------*)

open OUnit
open Printf

let test_equal msg u v =
  assert_equal
    ~msg
    ~cmp:(is_equal Float.delta)
    ~printer:to_string
    u v

let test_vector d =
  "Dim N" >::: [
  ]

let test_vector2 =
  "Dim 2" >::: [
    "swizzle2'" >:: begin fun _ ->
      let v = random_unit N._2 in
      let u =
	swizzle2 `ny`x 
	  (swizzle2 `y`nx v)
      in
      test_equal
	"swizzled^2 == id"
	u v
    end
  ]

let test_vector3 =
  "Dim 3" >::: [
    "swizzle3'" >:: begin fun _ ->
      let v = random_unit N._3 in
      let u =
	swizzle3 `nx`ny`nz
	  (swizzle3 `z`x`ny
	     (swizzle3 `nz`x`y
		(swizzle3 `z`nx`y v)))
      in
      test_equal
	"swizzle^3 == id"
	u v
    end
  ]

let test_vector4 =
  "Dim 4" >::: [
    "swizzle4'" >:: begin fun _ ->
      let v = random_unit N._4 in
      let u =
	swizzle4 `nx`ny`nz`nw
	  (swizzle4 `z`w`x`y 
	     (swizzle4 `nw`nz`ny`nx 
		(swizzle4 `y`x`w`z v)))
      in
      test_equal
	"swizzled^4 == id"
	u v
    end
  ]

let unit_test =
  "Vector" >::: [
    test_vector N._1;
    test_vector N._2;
    test_vector N._3;
    test_vector N._4;
    test_vector2;
    test_vector3;
    test_vector4;
    "TODO" >:: fun _ -> todo "More assertions!";
  ]

(*----------------------------------------------------------------------------*)
