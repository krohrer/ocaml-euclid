(** Affine transformations *)

type ephemeral = Core.ephemeral
type persistent = Core.persistent

type ('i,'j) t (** 'i Vector -> 'j Vector.t *)

val identity : ('i,'i) t

(*----------------------------------------------------------------------------((
LEFT        / 1,1 ... 1,I \     / 1 \      / 1 \
           |   .       .   |   |  .  |    |  .  |
           |   .       .   | * |  .  | == |  .  |
           |   .       .   |   |  .  |    |  .  |
            \ J,1 ... J,I /     \ I /      \ J /

expr   :          M          *    V    ->    V'
dim's  :           j,i             i          j
))----------------------------------------------------------------------------*)

val matrix_left  : (persistent,'j,'i) Matrix.t -> ('i,'j) t

(*----------------------------------------------------------------------------((
RIGHT                   / 1,1 ... 1,J \   
                       |   .       .   |  
           ( 1...I ) * |   .       .   | == ( 1...J )
                       |   .       .   |  
                        \ I,1 ... I,J /   

expr   :       V     *        L          ->     V'
m.not. :        i              i,j               j
))----------------------------------------------------------------------------*)

val matrix_right : (persistent,'i,'j) Matrix.t -> ('i,'j) t

(*----------------------------------------------------------------------------((
  Q * V * inv(Q) == V'
       3             3
))----------------------------------------------------------------------------*)

val quaternion_rotation  : persistent Quaternion.t -> (N._3,N._3) t

(*----------------------------------------------------------------------------((
  V + T == V'
   i   i    i
))----------------------------------------------------------------------------*)

val translation : (persistent,'i) Vector.t -> ('i,'i) t

(*----------------------------------------------------------------------------((

  AT1    . AT2    == AT'
     i,j      k,i      k,j

))----------------------------------------------------------------------------*)

val compose : ('j,'k) t -> ('i,'j) t -> ('i,'k) t
val concat  : ('i,'j) t -> ('j,'k) t -> ('i,'k) t

val invert : ('i,'i) t -> ('i,'i) t

val apply : ('i,'j) t -> (persistent,'i) Vector.t -> (_,'j) Vector.t
val call' : ('i,'j) t -> (persistent,'i) Vector.t -> out:(ephemeral,'j) Vector.t -> unit

(** apply *)
val transform  : ('i,'j) t -> (persistent,'i) Vector.t -> (_,'j) Vector.t
(** call' *)
val transform' : ('i,'j) t -> (persistent,'i) Vector.t -> (ephemeral,'j) Vector.t -> unit
