open Base

type t [@@derving show, compare]

val from_name : string -> t option
(** if the name is a registered tag then return some t otherwise none *)

val name : t -> string

val equal : t -> t -> bool

val greater_than : t -> t -> bool

val greater_than_or_equal : t -> t -> bool

val less_than : t -> t -> bool

val less_than_or_equal : t -> t -> bool

val create : int -> string -> int -> t

val update_count : ?incr:int -> string -> unit Or_error.t
