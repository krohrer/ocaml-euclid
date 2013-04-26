type zero
type 'a t = int
type 'a succ

type 'a succ0 = 'a
type 'a succ1 = 'a succ
type 'a succ2 = 'a succ succ
type 'a succ3 = 'a succ succ succ
type 'a succ4 = 'a succ succ succ succ
type 'a succ5 = 'a succ succ succ succ succ
type 'a succ6 = 'a succ succ succ succ succ succ
type 'a succ7 = 'a succ succ succ succ succ succ succ
type 'a succ8 = 'a succ succ succ succ succ succ succ succ
type 'a succ9 = 'a succ succ succ succ succ succ succ succ succ
type 'a succ10 = 'a succ succ succ succ succ succ succ succ succ succ

type _0 = zero succ0
type _1 = zero succ1
type _2 = zero succ2
type _3 = zero succ3
type _4 = zero succ4
type _5 = zero succ5
type _6 = zero succ6
type _7 = zero succ7
type _8 = zero succ8
type _9 = zero succ9
type _10 = zero succ10

(*------------------------------------*)

let unary k = k 0
let i n k = k (n + 1)
let num n = n
let int d = d
let __num_of_int__ d = d

let _0 = __num_of_int__ 0
let _1 = __num_of_int__ 1
let _2 = __num_of_int__ 2
let _3 = __num_of_int__ 3
let _4 = __num_of_int__ 4
let _5 = __num_of_int__ 5
let _6 = __num_of_int__ 6
let _7 = __num_of_int__ 7
let _8 = __num_of_int__ 8
let _9 = __num_of_int__ 9
let _10 = __num_of_int__ 10

let pred n = n-1
let succ n = n+1

(*------------------------------------*)

let print fmt n =
  Format.pp_print_int fmt n

(*----------------------------------------------------------------------------*)
