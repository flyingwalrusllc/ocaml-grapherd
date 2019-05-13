open Core

type t = Empty | Weight of float [@@deriving show, yojson]

let of_float f = Weight f

let to_float w = match w with Empty -> None | Weight w -> Some w

let empty = Empty

let compare a b =
  match (a, b) with
  | (Empty, Empty) -> 0
  | (Weight wa, Weight wb) -> Float.compare wa wb
  | (Empty, Weight _) -> -1
  | (Weight _, Empty) -> 1

let equal a b =
  Int.equal (compare a b) 0
         
