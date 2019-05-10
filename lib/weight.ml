type t = Empty | Weight of float [@@deriving show, yojson]

let of_float f = Weight f

let to_float w = match w with Empty -> None | Weight w -> Some w

let empty = Empty
