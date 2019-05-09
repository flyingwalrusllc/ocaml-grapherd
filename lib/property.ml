open Core
   
type t = int [@@deriving show]

let of_int i = i

let to_int l = l

let equal a b =
  Int.equal (to_int a) (to_int b)
