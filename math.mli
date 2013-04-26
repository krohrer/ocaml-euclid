(** Math library

    Contains modules for linear algebra (vector, matrices) and some
    useful stuff for computer graphics, like quaternions and an
    abstract angle data type.
*)

val binsearchi : f:(int -> float) -> x0:int -> x1:int -> float -> int
val binsearchf : ?eps:float -> ?n:int -> f:(float -> float) -> x0:float -> x1:float -> float -> float
