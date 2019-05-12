(** edges are directed links from vertex a to other vertices if a
   graph is undirected then when adding an edge it needs to be added
   to the target and recipient both *)

(** An edge is a label with some other data need for various graph
   traversals. *)
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
(** [compare a b] compare two edges.

 Edge identity is based on label. If the labels are the same then it's
 the same edge even if the weights and/or
 properties differ.The empty edge only
 equals itself and is less than all other
 edges *)
