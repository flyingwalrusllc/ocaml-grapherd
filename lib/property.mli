(** for graphs where edges have properties this module is the type and
   operations on that property type *)

type t [@@deriving show]
(** whatever a property is actually represented by is obscured by this type *)

val of_int : int -> t
(** create a property from an int *)

val equal : t -> t -> bool
(** is this property the same as that property *)
                        
