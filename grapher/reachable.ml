open Core
   
type t = int

let of_int i = i

let to_int l = l

let compare a b = compare (to_int a) (to_int b)

let exceeds r d = (to_int r) <= d


                                 
