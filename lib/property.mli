(** for graphs where edges have properties this module is the type and
   operations on that property type *)

(** whatever a property is actually represented by is obscured by this type *)
type t [@@deriving show, yojson]

val of_int : int -> t
(** create a property from an int *)

val equal : t -> t -> bool
(** is this property the same as that property *)
