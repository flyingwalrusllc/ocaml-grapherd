type t [@@deriving show, yojson]

val of_float : float -> t

val to_float : t -> float option

val empty : t
