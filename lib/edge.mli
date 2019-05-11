(** edges are directed links from vertex a to other vertices if a
   graph is undirected then when adding an edge it needs to be added
   to the target and recipient both *)

type t [@@deriving show, yojson]

val empty : t
(** the empty edge *)

val create : ?weight:Weight.t -> ?props:Property.t list -> Label.t -> t
(** all edges have labels which are the id's of other vertices *)

val label : t -> Label.t
(** extract this edges label *)

val weight : t -> Weight.t
(** extract this edges weight *)

val properties : t -> Property.t list
(** extract this edges properties *)

val equal : t -> t -> bool
(** do these two edges have the same label? *)

val compare : t -> t -> int
(** compare by label *)
