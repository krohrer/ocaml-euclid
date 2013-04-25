(* Kaspar Rohrer, Tue Apr 20 00:30:56 CEST 2010 *)

open Core

module Doc =
struct
  let name = "Math"
  let version = 0, 1, 0
  let description = "The Math libary provides mathematical constructs like vectors, matrices, quaternions, etc. which are commonly used in the field of computer graphics."

  let author = kmr
  let contributors = []
  let maintainer = Some kmr
end

module Log = MakeLog(Doc)

let binsearchi ~f ~x0 ~x1 y =
  let rec iter x0 x1 =
    let xm = (x0 + x1) / 2 in
      if x0 <> xm && xm <> x1 then (
	(* to boldly go ... *)
	let ym = f xm in
	  if ym = y then
	    (* bingo *)
	    xm
	  else
	    if ym < y then
	      (* we are lower than expected *)
	      iter xm x1 (* search upper half *)
	    else
	      (* we are higher than expected *)
	      iter x0 xm (* search lower half *)
      )
      else (
	(* we have exhausted the search space, return best fit. *)
	let y0 = f x0 and y1 = f x1 in
	let distance a b = let x = a -. b in x *. x in
	  if distance y y0 < distance y y1 then
	    x0
	  else
	    x1
      )
  in
    assert (x0 <= x1);
    let y0 = f x0 and y1 = f x1 in
      if y < y0 then
	x0
      else
	if y1 < y then
	  x1
	else
	  iter x0 x1

let binsearchf ?(eps=Flt.epsilon) ?(n=53) ~f ~x0 ~x1 y =
  let rec iter x0 x1 i =
    let xm = (x0 +. x1) *. 0.5 in
      if x0 < xm && xm < x1 && i < n then (
	(* to boldly go ... *)
	let ym = f xm in
	  match Flt.compare ~eps ym y with
	    | `L  -> iter xm x1 (i + 1)
	    | `EQ -> xm
	    | `G  -> iter x0 xm (i + 1)
      )
      else (
	(* we have exhausted the search space, return best fit. *)
	let y0 = f x0 and y1 = f x1 in
	let distance a b = let x = a -. b in x *. x in
	  if distance y y0 < distance y y1 then
	    x0
	  else
	    x1
      )
  in
    assert (x0 <= x1);
    let y0 = f x0 and y1 = f x1 in
      if y < y0 then
	x0
      else
	if y1 < y then
	  x1
	else
	  iter x0 x1 0
