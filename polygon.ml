type ephemeral = Core.ephemeral
type persistent = Core.persistent

open Core
module V = Vector

module type S =
sig
  type d
  type t = (persistent,d) V.t array
end

type 'd t = (persistent,'d) V.t array

let of_array pa = pa
let to_array pa = pa
let of_list pl = Array.of_list pl

let count pg = Array.length pg
let ith pg i = pg.(i)

let iter f pg =
  let n = Array.length pg in
  for i = 0 to n - 1 do
    f pg.(i)
  done
    
let iter_edges f pg =
  let n = Array.length pg in
    if n > 0 then
      for i = 0 to n - 2 do
	f pg.(i) pg.(i + 1)
      done;
      f pg.(n - 1) pg.(0)

let fold f a pg =
  let n = Array.length pg in
  let rec folder i a =
    if i < n then
      let a' = f a pg.(i) in
	folder (i + 1) a'
    else
      a
  in
    folder 0 a

let fold_edges f a pg =
  let n = Array.length pg in
  let rec folder prev i a =
    if i < n then
      let curr = pg.(i) in
      let a' = f a prev curr in
	folder curr (i + 1) a'
    else
      a
  in
    if n > 0 then
      let a' = folder pg.(0) 1 a in
	f a' pg.(n-1) pg.(0)
    else
      a

let print fmt pg =
  print_array V.print fmt pg
