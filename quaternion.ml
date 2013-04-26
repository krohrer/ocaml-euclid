(*----------------------------------------------------------------------------*)

(* TODO : assertions for mutating functions to prevent mangling of objects 
   which are input as well as output arguments.
   e.g.: Matrix.mulm', Matrix.invert' *)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

open Core

type 'a t = ('a,N._4) Vector.t

module V = Vector
module M = Matrix

(*----------------------------------------------------------------------------*)

let make ?(real=0.) ?(i=0.) ?(j=0.) ?(k=0.) () =
  V.make4 i j k real

(*------------------------------------*)

let copied = V.copied
let zeroed = V.zeroed

let copy' = V.copy'
let zero' = V.zero'

let copy_to_array' = V.copy_to_array'
let copy_from_array' = V.copy_from_array'

let zero = V.zero N._4
let identity = V.ew N._4
let identity' q = V.ew' q

let random_unit ?(state=Rnd.default_state) () =
  V.random_unit ~state N._4

(*------------------------------------*)

let of_vector v = v
let to_vector v = v

let of_array = V.of_array N._4
let to_array q = V.to_array q

let __absolve__ q = V.__absolve__ q
let __magic__ q = V.__magic__ q

(*------------------------------------*)

let real q = V.w q
let i q = V.x q
let j q = V.y q
let k q = V.z q
let imag q = V.__absolve__ (V.of_array N._3 [|i q; j q; k q|])

let get_imag' q dst =
  V.xfer' q dst

let set_real' q r =
  V.set_w' q r

let set4' q ~real ~i ~j ~k =
  V.set_x' q i;
  V.set_y' q j;
  V.set_z' q k;
  V.set_w' q real

let set_imag' q im =
  V.set_x' q (V.x im);
  V.set_y' q (V.y im);
  V.set_z' q (V.z im)

let set_i' q i = V.set_x' q i
let set_j' q j = V.set_y' q j
let set_k' q k = V.set_z' q k

(*------------------------------------*)

let ei = make ~i:1. ()
let ej = make ~j:1. ()
let ek = make ~k:1. ()

let ei' q = set4' q ~real:0. ~i:1. ~j:0. ~k:0.
let ej' q = set4' q ~real:0. ~i:0. ~j:1. ~k:0.
let ek' q = set4' q ~real:0. ~i:0. ~j:0. ~k:1.

let to_matrix' (q:'a t) (a:(ephemeral,N._3,N._3) Matrix.t) =
  let r = real q and 
      i = i q and 
      j = j q and 
      k = k q
  in
  let ii, jj, kk = i *. i, j *. j, k *. k in
  let ij, jk, ik = i *. j, j *. k, i *. k in
  let ir, jr, kr = i *. r, j *. r, k *. r in
    M.set' a 0 0 (1. -. 2.*.(jj +. kk));
    M.set' a 0 1 (      2.*.(ij -. kr));
    M.set' a 0 2 (      2.*.(ik +. jr));
    M.set' a 1 0 (      2.*.(ij +. kr));
    M.set' a 1 1 (1. -. 2.*.(ii +. kk));
    M.set' a 1 2 (      2.*.(jk -. ir));
    M.set' a 2 0 (      2.*.(ik -. jr));
    M.set' a 2 1 (      2.*.(jk +. ir));
    M.set' a 2 2 (1. -. 2.*.(ii +. jj))

let to_matrix q =
  let r = real q and 
      i = i q and
      j = j q and
      k = k q
  in
  let ii, jj, kk = i *. i, j *. j, k *. k in
  let ij, jk, ik = i *. j, j *. k, i *. k in
  let ir, jr, kr = i *. r, j *. r, k *. r in
    M.of_arrays N._3 N._3
      [|[|1. -. 2.*.(jj +. kk);       2.*.(ij -. kr);       2.*.(ik +. jr)|];
	[|      2.*.(ij +. kr); 1. -. 2.*.(ii +. kk);       2.*.(jk -. ir)|];
	[|      2.*.(ik -. jr);       2.*.(jk +. ir); 1. -. 2.*.(ii +. jj)|]|]

(*------------------------------------*)

(* XXX : Shouldn't the destination be the first argument? See Matrix
   or Vector for comparision. *)
let axis_rotation' axis angle dst =
  let ha = 0.5 *. Angle.to_radian angle in
  let real = cos ha and
      cimag = (sin ha)/.(V.magnitude axis)
  in
  let i = cimag *. V.x axis
  and j = cimag *. V.y axis
  and k = cimag *. V.z axis
  in
    set4' dst ~i ~j ~k ~real    

let axis_rotation axis angle =
  let q = make () in
    axis_rotation' axis angle q;
    V.__absolve__ q

(*------------------------------------*)

let ref_trans1 op1' q = 
  let c = V.zeroed q in
    op1' q c;
    __absolve__ c

let ref_trans2 op2' q r =
  let c = V.zeroed q in
    op2' q r c;
    __absolve__ c

(*------------------------------------*)

let conjugate' q dst =
  set_i' dst (-. i q);
  set_j' dst (-. j q);
  set_k' dst (-. k q);
  set_real' dst (real q)

let conjugated q =
  let real = real q and i = -.(i q) and j = -.(j q) and k = -.(k q) in
    make ~real ~i ~j ~k ()

let inverse' q dst =
  conjugate' q dst;
  V.invert' dst dst

let inversed q = ref_trans1 inverse' q

let slerp' ~t q r dst = assert false

let slerped ~t q r = ref_trans2 (slerp' ~t) q r

let mulq' q r dst =
  let q0 = i q and q1 = j q and q2 = k q and q3 = real q in
  let r0 = i r and r1 = j r and r2 = k r and r3 = real r in
  let i = q3*.r0 +. r3*.q0 +. (q1*.r2 -. q2*.r1)
  and j = q3*.r1 +. r3*.q1 +. (q2*.r0 -. q0*.r2)
  and k = q3*.r2 +. r3*.q2 +. (q0*.r1 -. q1*.r0)
  and real = r3*.r3 -. q0*.r0 -. q1*.r1 -. q2*.r2
  in
    set4' dst ~i ~j ~k ~real

let mulq q r =
  let q0 = i q and q1 = j q and q2 = k q and q3 = real q in
  let r0 = i r and r1 = j r and r2 = k r and r3 = real r in
  let i = q3*.r0 +. r3*.q0 +. (q1*.r2 -. q2*.r1) and
      j = q3*.r1 +. r3*.q1 +. (q2*.r0 -. q0*.r2) and
      k = q3*.r2 +. r3*.q2 +. (q0*.r1 -. q1*.r0) and 
      real = r3*.r3 -. q0*.r0 -. q1*.r1 -. q2*.r2
  in
    make ~real ~i ~j ~k ()

let concatenate' q s dst =
  mulq' q s dst;
  V.normalize' dst dst

let concatenated q r =
  let v = mulq q r in
    V.normalize' v v;
    __absolve__ v

let neg' = V.neg'
let sub' = V.sub'
let add' = V.add'
let mul1' = V.mul1'

let neg = V.neg
let sub = V.sub
let add = V.add
let mul1 = V.mul1

let normalize' = V.normalize'
let magnitude = V.magnitude
let magnitude_squared = V.magnitude_squared

let normalized = V.normalized

(*------------------------------------*)

let is_equal ~eps q r = V.is_equal ~eps q r

(*------------------------------------*)

open Format
open Hopp

let print fmt q =
  fprintf fmt "@[<1>{r = %g;@ i = %g;@ j = %g;@ k = %g}@]"
    (real q) (i q) (j q) (k q)

let to_string q = pp_make_to_string print q

(*----------------------------------------------------------------------------*)

open OUnit
open Printf

let test_equal msg a b =
  (* Needs cast because of OUnit.assert_equal / (=) *)
  let a = __magic__ a in
    OUnit.assert_equal
      ~msg
      ~cmp:(is_equal ~eps:Flt.delta)
      a b

let unit_test =
  "Quaternion" >::: [
    "axis_rotation" >:: begin fun _ ->
      let q = axis_rotation (V.make3 2. 1. 3.) (Angle.degree 65.) in
      let r = axis_rotation (V.make3 2. 1. 3.) (Angle.degree (-65.)) in
	Flt.test_equal "(magnitude q) 1." (magnitude q) 1.;
	Flt.test_equal "(magnitude r) 1." (magnitude r) 1.;
	test_equal "(conjugated q) r" (conjugated q) r
    end;
    "random_unit, normalized" >:: begin fun _ ->
      let q = random_unit () in
      let r = normalized q in
	test_equal "random_unit should not change when normalized" q r
    end;
   "TODO" >:: fun _ -> todo "More assertions!";
  ]

(*----------------------------------------------------------------------------*)
