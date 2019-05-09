type t  [@@deriving show, yojson]

val of_float : float -> t

val to_float : t -> float option

val equal : t -> t -> bool
                        
val empty : t
