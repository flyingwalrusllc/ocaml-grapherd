(** labels are simple type aliases that identify individual vertices *)
type t [@@deriving show]

val of_int : int -> t

val to_int : t -> int

val equal : t -> t -> bool

val empty : t
