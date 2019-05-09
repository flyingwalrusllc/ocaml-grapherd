type t [@@derving show]

val from_string : string -> t

val to_string : t -> string

val compare : t -> t -> int

val equal : t -> t -> bool

val greater_than : t -> t -> bool

val greater_than_or_equal : t -> t -> bool

val less_than : t -> t -> bool

val less_than_or_equal : t -> t -> bool

val tag_of_tuple : int * string * int -> t
