type t [@@deriving show, yojson]

val create : string -> float -> int -> t

val update : t -> float -> t

val feature : t -> bool
