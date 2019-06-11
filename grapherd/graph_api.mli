open Base
open Grapher

val all_shortest_paths :
     'a Graph.t
  -> Graph.Label.t
  -> Graph.Label.t
  -> int
  -> Graph.Label.t list list Or_error.t
(** [all_shortest_paths start goal reachable] get all paths between
  * the start and goal vertices sorted by length and limitted to a
  * length of reachable
  *)

val k_reachable :
  'a Graph.t -> Graph.Label.t -> int -> Graph.Label.t list array
(** [k_reachable graph label k] return an array of reachables for distances 0 to k
 *
 * reachable 0 is the start node
 * reachable 1 is all the edges of the start node
* reachable n is  *)
