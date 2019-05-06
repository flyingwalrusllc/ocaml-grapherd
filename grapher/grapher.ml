
open Core
   
module S = Graph.Make_graph(Vertex.Vertex_set)
         
open Vertex.Label

let max_k = 4

let get_labels graph start = List.map (S.edges graph start) ~f:(fun e -> Vertex.Edge.label e)

let k_reachable graph start target distance =
  let limit = if Reachable.exceeds distance max_k then Reachable.of_int max_k else distance in
  let rec next candidate path found =
    let labels = get_labels graph candidate in
    let depth = Path.length path in
    if Reachable.exceeds limit depth then found else
      List.concat (List.map labels
                     ~f:(fun l ->
                       if l <> start then
                         let target_path = Path.append path l in
                         if l == target then
                           target_path :: found
                         else next l target_path found
                       else
                         found))
  in
  next start (Path.empty start) []

let%test "k_reachable" =
  let graph = S.create 25 in
  let _ = S.add_edges graph (Vertex.Label.of_int 10)
            [ Vertex.Edge.create (Vertex.Label.of_int 12);
              Vertex.Edge.create (Vertex.Label.of_int 15); ] in
  let _ = S.add_edges graph (Vertex.Label.of_int 12) 
            [ Vertex.Edge.create (Vertex.Label.of_int 10);
              Vertex.Edge.create (Vertex.Label.of_int 11); ] in
  let _ = S.add_edges graph (Vertex.Label.of_int 15) 
            [ Vertex.Edge.create (Vertex.Label.of_int 10);
              Vertex.Edge.create (Vertex.Label.of_int 11); ] in
  let _ = S.add_edges graph (Vertex.Label.of_int 11) 
            [ Vertex.Edge.create (Vertex.Label.of_int 12);
              Vertex.Edge.create (Vertex.Label.of_int 15); ] in
  let reachable = k_reachable graph (Vertex.Label.of_int 10) (Vertex.Label.of_int 11) (Reachable.of_int 3) in
  Int.equal (List.length reachable) 2 
