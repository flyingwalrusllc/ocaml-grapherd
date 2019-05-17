open Core
open Bigarray

type 'a t =
  { mutable arr: (int, int_elt, c_layout) Array1.t
  ; mutable length: int
  ; to_int: 'a -> int
  ; of_int: int -> 'a }

let of_array ar to_int of_int =
  let int_arr = Array.map ar ~f:(fun a -> to_int a) in
  { arr= Array1.of_array Bigarray.Int c_layout int_arr
  ; length= Array.length ar
  ; to_int
  ; of_int }

let get da i = da.of_int (Array1.get da.arr i)

let set da i v = Array1.set da.arr i (da.to_int v)

let length da = da.length

let grow g id =
  let rec growth_amount current at_least =
    if current > at_least then current
    else growth_amount (current * 2) at_least
  in
  let n_size = growth_amount g.length id in
  let growth = n_size - g.length in
  if growth > 0 then (
    let n_arr = Array1.create Bigarray.Int c_layout n_size in
    let _ = Array1.blit g.arr n_arr in
    g.arr <- n_arr ;
    g.length <- n_size ;
    g )
  else g
