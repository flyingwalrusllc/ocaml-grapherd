open Core

type t = int [@@deriving show, yojson]

let of_int i = i
(** [of_int i] lift an int into a Reachable *)

let to_int l = l
(** [to_int r] extract an int from a Reachable *)

let compare a b = compare (to_int a) (to_int b)
(** [compare r1 r2] compare two reachables and return < 0 if r1 is
   less than r2, return 0 if they're equal and return > 1 if r1 is
   greater than r2 *)

let succ r = r + 1
(** [succ r] return the next higher Reachable.t *)

let pred r = r - 1
(** [succ r] return the next lesser Reachable.t *)
