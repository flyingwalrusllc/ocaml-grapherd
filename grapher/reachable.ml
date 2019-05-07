open Core

type t = int [@@deriving show]

(** [of_int i] lift an int into a Reachable *)
let of_int i = i

(** [to_int r] extract an int from a Reachable *)
let to_int l = l

(** [compare r1 r2] compare two reachables and return < 0 if r1 is
   less than r2, return 0 if they're equal and return > 1 if r1 is
   greater than r2 *)
let compare a b = compare (to_int a) (to_int b)

(** [succ r] return the next higher Reachable.t *)
let succ r = r + 1

(** [succ r] return the next lesser Reachable.t *)
let pred r = r - 1
