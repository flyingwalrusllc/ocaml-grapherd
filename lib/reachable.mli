type t [@@deriving show, eq]

val of_int : int -> t

val to_int : t -> int

val compare : t -> t -> int

val succ : t -> t

val pred : t -> t
