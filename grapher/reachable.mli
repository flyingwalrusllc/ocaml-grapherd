type t

val of_int : int -> t

val to_int : t -> int

val compare : t -> t -> int

val exceeds : t -> int -> bool
