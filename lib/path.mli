(** a simple type alias for the element type of paths between vertices *)
type t [@@deriving show]

val append : t -> Label.t -> t

val empty : Label.t -> t

val length : t -> int


