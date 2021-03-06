type ephemeral = Core.ephemeral
type persistent = Core.persistent

(* TODO: Rewrite this using GADT *)

type ('i,'j) t = {
  forward : ('i,'j) t';
  inverse : ('j,'i) t' Lazy.t;
}
and  ('i,'j) t' =
  | Identity
  | Matrix of (persistent,'i,'j) Matrix.t
  | QRot of persistent Quaternion.t
  | Tlt of (persistent,'i) Vector.t
  | Composition of composition_t
  | Inverse of ('j,'i) t'
  | Full of (persistent, 'i,'j) Matrix.t
  | Lazy of ('j,'i) t Lazy.t
and ('i,'k) composition_t = {
  lside : 'j . ('j,'k) t;
  rside : 'j . ('i,'j) t;
}

let compose f g =
  match f g with
  | Identity, x -> x
  | x, Identity -> x
  | RMatrix m, RMatrix n -> Matrix.mulm m n
  | LMatrix m, LMatrix n -> Matrix.mulm m n
  | Quaternion q, Quaternion r -> Quaternion.mulq q r
  | Translation u, Translation v -> lazy (Translation (Vector.add u v))
  | _, _ -> Composition


let rec invert lt = Inverse lt
