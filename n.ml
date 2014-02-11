type zero
type 'a t = int
type 'a s

type 'a s0 = 'a
type 'a s1 = 'a s
type 'a s2 = 'a s s
type 'a s3 = 'a s s s
type 'a s4 = 'a s s s s
type 'a s5 = 'a s s s s s
type 'a s6 = 'a s s s s s s
type 'a s7 = 'a s s s s s s s
type 'a s8 = 'a s s s s s s s s
type 'a s9 = 'a s s s s s s s s s
type 'a s10 = 'a s s s s s s s s s s

type _0 = zero s0
type _1 = zero s1
type _2 = zero s2
type _3 = zero s3
type _4 = zero s4
type _5 = zero s5
type _6 = zero s6
type _7 = zero s7
type _8 = zero s8
type _9 = zero s9
type _10 = zero s10

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
let s n = n+1

(*------------------------------------*)

let print fmt n =
  Format.pp_print_int fmt n

(*----------------------------------------------------------------------------*)
