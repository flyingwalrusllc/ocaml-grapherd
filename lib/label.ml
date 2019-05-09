open Core

type t = Empty | Valid of int [@@deriving show]

let of_int i = Valid i

let to_int l = match l with Empty -> 0 | Valid l -> l

let equal a b =
  match (a, b) with
  | Empty, Empty ->
      true
  | Valid x, Valid y ->
      Int.equal x y
  | _, _ ->
      false

let empty = Empty
