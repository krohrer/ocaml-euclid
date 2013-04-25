exception Conversion of string
exception Break
exception Canceled

(*------------------------------------*)

type ephemeral
type persistent

(*------------------------------------*)

module Option =
struct
  type 'a t = 'a option

  let maybe fo x =
    match fo with
      | Some f -> f x
      | None -> ()

  let may f x =
    match x with None -> ()
      | Some x -> let _ = f x in ()

  let may_map f x =
    match x with None -> None
      | Some x -> Some (f x)

  let default x opt =
    match opt with None -> x | Some y -> y

  let may_default f x opt =
    match opt with None -> f x | Some y -> y

  let print pr fmt =
    function
      | None -> Format.pp_print_string fmt "None"
      | Some v -> ()
end
