open Core

type t = int [@@deriving show, yojson]

let of_int p = p

let to_int p = p

let equal a b = Int.equal (to_int a) (to_int b)
