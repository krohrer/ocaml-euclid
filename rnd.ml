type state = Random.State.t

let make_state = Random.State.make_self_init
let make_state_with_seed = Random.State.make

let default_state = make_state ()

let copy = Random.State.copy
let int = Random.State.int
let float = Random.State.float
let bool = Random.State.bool

module BoxMueller =
  struct
    let stash = [|nan|]

    let rec refill s = 
      let x1 = float s 2. -. 1. in
      let x2 = float s 2. -. 1. in
      let w = x1*.x1 +. x2*.x2 in
      if w < 1.0 then 
	let w = sqrt ( (-2. *. log w) /. w) in
	stash.(0) <- x1 *. w;
	x2 *. w
      else
	refill s

    let float s =
      if stash.(0) == nan then
	refill s
      else
	let r = stash.(0) in
	stash.(0) <- nan;
	r
  end

let gaussian s ?(mu=0.) ?(sigma=1.) () =
  sigma *. (BoxMueller.float s) +. mu
