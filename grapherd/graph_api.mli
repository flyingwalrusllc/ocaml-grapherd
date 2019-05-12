open Core
open Grapher
   
module S : Graph.Graph

val all_shortest_paths :
  S.t -> Label.t -> Label.t -> Reachable.t -> Path.t list Or_error.t
(** [all_shortest_paths start goal reachable] get all paths between
  * the start and goal vertices sorted by length and limitted to a
  * length of reachable
  *)

val k_reachable : S.t -> Label.t -> Reachable.t -> Label.t list array
(** [k_reachable graph label k] return an array of reachables for distances 0 to k
 *
 * reachable 0 is the start node
 * reachable 1 is all the edges of the start node
* reachable n is  *)
