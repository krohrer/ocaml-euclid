(** Higher-order pretty printing

    Open the module to use it.
*)

val pp_open_lbox : Format.formatter -> string -> unit
val pp_open_lhbox : Format.formatter -> string -> unit
val pp_open_lvbox : Format.formatter -> string -> unit
val pp_open_lhvbox : Format.formatter -> string -> unit
val pp_open_lhovbox : Format.formatter -> string -> unit
val pp_close_lbox : Format.formatter -> string -> unit

val pp_print_separator : Format.formatter -> string -> unit

val pp_print_abstract : Format.formatter -> 'a -> unit
val pp_print_quoted : Format.formatter -> string -> unit

val pp_print_labeled_field : string -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
val pp_print_list : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val pp_print_array : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a array -> unit
val pp_print_option : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit

val pp_make_to_string : (Format.formatter -> 'a -> unit) -> 'a -> string

