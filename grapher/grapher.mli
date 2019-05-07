module S : Graph.Graph

val all_shortest_paths :
  S.t -> Vertex.Label.t -> Vertex.Label.t -> Reachable.t -> Path.t list
(** [all_shortest_paths label label reachable] get all paths between
  * the first and second vertices
  *
  * the paths are generated via a depth
  * first search and are limited in length to reachable distance.  *)

val k_reachable :
  S.t -> Vertex.Label.t -> Reachable.t -> Vertex.Label.t list array
(** [k_reachable graph label k] return an array of reachables for distances 0 to k *)
