module S : Graph.Graph
   
val k_reachable :  S.t -> Vertex.Label.t -> Vertex.Label.t -> Reachable.t -> Path.t list
