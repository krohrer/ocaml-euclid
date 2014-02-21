type zero
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

type 'a t = int

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

let int d = d
let __num_of_int__ d = d

let unary k = k 0
let i n k = k (n + 1)
let num n = n

let _0	= unary num
let _1	= unary i num
let _2	= unary i i num
let _3	= unary i i i num
let _4	= unary i i i i num
let _5	= unary i i i i i num
let _6	= unary i i i i i i num
let _7	= unary i i i i i i i num
let _8	= unary i i i i i i i i num
let _9	= unary i i i i i i i i i num
let _10 = unary i i i i i i i i i i num

let pred n = n-1
let s n = n+1

module Pack2 =
  struct
    type ('a,'b) p = int
    let make n1 n2 =
      assert (n1 <= 0xFF && n2 <= 0xFF);
      (n2 lsl 8) lor n1

    let fst nn =
      nn land 0xFF

    let snd nn =
      (nn lsr 8) land 0xFF
  end

module Pack3 =
  struct
    type ('a,'b,'c) p = int
    let make n1 n2 n3 =
      assert (n1 <= 0xFF && n2 <= 0xFF && n3 <= 0xFF);
      (n3 lsl 16) lor (n2 lsl 8) lor n1

    let fst nn =
      nn land 0xFF

    let snd nn =
      (nn lsr 8) land 0xFF

    let trd nn =
      (nn lsr 16) land 0xFF
  end

(*------------------------------------*)

let print fmt n =
  Format.pp_print_int fmt n

(*----------------------------------------------------------------------------*)
