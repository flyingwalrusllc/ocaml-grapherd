
type t

val append : t -> Vertex.Label.t -> t

val empty : Vertex.Label.t -> t

val length : t -> int

val print : t -> unit

val print_path_list : t list -> unit
