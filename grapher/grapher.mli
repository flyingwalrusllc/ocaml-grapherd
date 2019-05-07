module S : Graph.Graph

val all_shortest_paths :
  S.t -> Vertex.Label.t -> Vertex.Label.t -> Reachable.t -> Path.t list
