type t = float array

let words_per_float = 64 / Sys.word_size

let alloc ~n =
  let words = n * words_per_float in
  Obj.obj (Obj.new_block Obj.double_array_tag words)

let init ~n ~dst f =
  for i = 0 to n-1 do dst.(i) <- f i done

let fill ~n ~dst s =
  for i = 0 to n-1 do dst.(i) <- s done

let unity ~n ~dst k =
  for i = 0 to n-1 do dst.(i) <- if i = k then 1. else 0. done

let neg ~n ~dst a =
  for i = 0 to n-1 do dst.(i) <- (~-.) a.(i) done

let add ~n ~dst a b =
  for i = 0 to n-1 do dst.(i) <- a.(i) +. b.(i) done

let sub ~n ~dst a b =
  for i = 0 to n-1 do dst.(i) <- a.(i) -. b.(i) done

let mul ~n ~dst a b =
  for i = 0 to n-1 do dst.(i) <- a.(i) *. b.(i) done

let smul ~n ~dst s a =
  for i = 0 to n-1 do dst.(i) <- s *. a.(i) done

let div ~n ~dst a b =
  for i = 0 to n-1 do dst.(i) <- a.(i) /. b.(i) done

let divs ~n ~dst a s =
  smul ~n ~dst (1./.s) a

let abs ~n ~dst a =
  for i = 0 to n-1 do dst.(i) <- Float.abs a.(i) done

let min ~n ~dst a b =
  for i = 0 to n-1 do dst.(i) <- Float.min a.(i) b.(i) done

let max ~n ~dst a b =
  for i = 0 to n-1 do dst.(i) <- Float.max a.(i) b.(i) done

let dot ~n a b =
  match n with
  | 1 -> a.(0)*.b.(0)
  | 2 -> a.(0)*.b.(0) +.a.(1)*.b.(1)
  | 3 -> a.(0)*.b.(0) +.a.(1)*.b.(1) +.a.(2)*.b.(2)
  | 4 -> a.(0)*.b.(0) +.a.(1)*.b.(1) +.a.(2)*.b.(2) +.a.(3)*.b.(3)
  | n ->
    let rec iter i sum =
      if i < n then iter (i+1) (sum +. a.(i)*.b.(i)) else sum
    in
    iter 0 0.

let lensq ~n a = dot ~n a a
let len ~n a = sqrt (lensq ~n a)

let cross ~dst a b =
  dst.(0) <- a.(1)*.b.(2) -. a.(2)*.b.(1);
  dst.(1) <- a.(2)*.b.(0) -. a.(0)*.b.(2);
  dst.(2) <- a.(0)*.b.(1) -. a.(1)*.b.(0)

let normalize ~n ~dst a =
  divs ~n ~dst a (len ~n a)

let mid ~n ~dst a b =
  for i = 0 to n-1 do
    dst.(i) <- 0.5 *. (a.(i) +. b.(i))
  done

let lerp ~n ~t ~dst a b =
  let omt = 1. -. t in
  for i = 0 to n-1 do
    dst.(i) <- omt*.a.(i) +. t*.b.(i)
  done

let minmax ~n ~dmin ~dmax a b =
  for i = 0 to n-1 do
    let ai = a.(i) and bi = b.(i) in
    if ai > bi then begin
      dmin.(i) <- bi; dmax.(i) <- ai
    end else begin
      dmin.(i) <- ai; dmax.(i) <- bi
    end
  done

let swizzle2 c0 c1 ~dst a =
  dst.(0) <- a.(c0);
  dst.(1) <- a.(c1)

let swizzle3 c0 c1 c2 ~dst a =
  dst.(0) <- a.(c0);
  dst.(1) <- a.(c1);
  dst.(2) <- a.(c2)

let swizzle4 c0 c1 c2 c3 ~dst a =
  dst.(0) <- a.(c0);
  dst.(1) <- a.(c1);
  dst.(2) <- a.(c2);
  dst.(3) <- a.(c3)

let mask2 c0 c1 ~dst a b =
  dst.(0) <- if c0 then a.(0) else b.(0);
  dst.(1) <- if c1 then a.(1) else b.(1)

let mask3 c0 c1 c2 ~dst a b =
  dst.(0) <- if c0 then a.(0) else b.(0);
  dst.(1) <- if c1 then a.(1) else b.(1);
  dst.(2) <- if c2 then a.(2) else b.(2)

let mask4 c0 c1 c2 c3 ~dst a b =
  dst.(0) <- if c0 then a.(0) else b.(0);
  dst.(1) <- if c1 then a.(1) else b.(1);
  dst.(2) <- if c2 then a.(2) else b.(2);
  dst.(3) <- if c3 then a.(3) else b.(3)

let random_unit state ~n ~dst =
  let rec search () =
    for i = 0 to n-1 do
      dst.(i) <- 1. -. Rnd.float state 2.
    done;
    let lsq = lensq ~n dst in
    if Float.(is_zero delta lsq) && lsq < 1. then
      divs ~n ~dst dst (sqrt lsq)
    else
      search ()
  in
  search ()

let random state ~n ~dst ~lower ~upper =
  for i = 0 to n-1 do
    let l = lower.(i) in
    let u = upper.(i) in
    dst.(i) <- Rnd.float state (u -. l) +. l
  done

let random_gaussian state ~n ~dst ~mu ~sigma =
  for i = 0 to n-1 do
    dst.(i) <- Rnd.gaussian state ~mu:mu.(i) ~sigma:sigma.(i) ()
  done
  
let is_equal eps ~n a b =
  try
    for i = 0 to n-1 do 
      if not (Float.is_equal eps a.(i) b.(i)) then
	raise Exit
    done;
    true
  with
    Exit -> false

let is_zero eps ~n a =
  try
    for i = 0 to n-1 do
      if not (Float.is_zero eps a.(i)) then
	raise Exit
    done;
    true
  with
    Exit -> false
