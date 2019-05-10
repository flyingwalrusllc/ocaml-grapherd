open Core

type t = Empty | Valid of int [@@deriving show, yojson]

let of_int i = Valid i

let to_int l = match l with Empty -> None | Valid l -> Some l

let equal a b = Int.equal (compare a b) 0

let compare a b =
  match (a, b) with
  | Empty, Empty ->
      0
  | Valid x, Valid y ->
      Int.compare x y
  | Valid _, Empty ->
      1
  | Empty, Valid _ ->
      -1

let empty = Empty
