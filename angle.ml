type rad
type deg
type 'a t = float

let half_pi = acos 0.
let pi = 2. *. half_pi
let tau = 4. *. half_pi

let half_circle = 180.
let full_circle = 360.

external rad : float -> rad t = "%identity"
external deg : float -> deg t = "%identity"

external rad_as_float : rad t -> float = "%identity"
external deg_as_float : deg t -> float = "%identity"

let rad_to_deg x = x *. 180. /. pi
let deg_to_rad x = x *. pi /. 180.

let normalize ~period a =
  let n = mod_float a period in
  if n >= 0. then n else n +. period

let min a b = if a <= b then a else b
let max a b = if a <= b then b else a

external ( ~- ) : 'a t -> 'a t = "%negfloat"
external ( +  ) : 'a t -> 'a t -> 'a t = "%identity"
external ( -  ) : 'a t -> 'a t -> 'a t = "%identity"
external ( *. ) : float-> 'a t -> 'a t = "%mulfloat"
external ( /. ) : 'a t -> float-> 'a t = "%divfloat"

external abs : 'a t -> 'a t = "%absfloat"

external acos : float -> rad t = "caml_acos_float" "acos" "float"
external asin : float -> rad t = "caml_asin_float" "asin" "float"
external atan : float -> rad t = "caml_atan_float" "atan" "float"
external atan2 : float -> float -> rad t = "caml_atan2_float" "atan2" "float"

external cos : rad t -> float = "caml_cos_float" "cos" "float"
external sin : rad t -> float = "caml_sin_float" "sin" "float"
external tan : rad t -> float = "caml_tan_float" "tan" "float"

external ( <  ) : 'a t -> 'a t -> bool = "%lessthan"
external ( >  ) : 'a t -> 'a t -> bool = "%greaterthan"
external ( <= ) : 'a t -> 'a t -> bool = "%lessequal"
external ( >= ) : 'a t -> 'a t -> bool = "%greaterequal"
external compare : 'a t -> 'a t -> int = "%compare"
