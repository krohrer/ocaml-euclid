(** Affine transformations (expression style with persistent values))
    with statically typed dimensions *)

type persistent = Core.persistent

type ('a,'m,'n) t

(*----------------------------------------------------------------------------*)
(** {6 Creation / Initialization} *)
(*----------------------------------------------------------------------------*)

val linear_
