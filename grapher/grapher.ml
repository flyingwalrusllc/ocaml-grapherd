open Core
module S = Graph.Make_graph (Vertex.Vertex_set)

let max_k = Reachable.of_int 4

let all_shortest_paths graph start target distance =
  let limit =
    if Reachable.compare distance max_k > 0 then max_k else distance
  in
  let rec next candidate path found =
    let labels = S.edge_labels graph candidate in
    let depth = Path.length path in
    if Reachable.compare (Reachable.of_int depth) limit > 0 then found
    else
      List.concat
        (List.map labels ~f:(fun l ->
             if l <> start then
               let target_path = Path.append path l in
               if Vertex.Label.equal l target then target_path :: found
               else next l target_path found
             else found ))
  in
  next start (Path.empty start) []

let k_reachable graph start k =
  let depth = if Reachable.compare k max_k > 0 then max_k else k in
  let reach_n graph labels =
    List.fold labels ~init:[] ~f:(fun acc label ->
        List.append (S.edge_labels graph label) acc )
  in
  let rec loop graph labels cur_depth depth reachable =
    if (Reachable.compare cur_depth depth) >= 0 then reachable
    else
      let ll =
        List.map labels ~f:(fun label ->
            reach_n graph (S.edge_labels graph label) )
      in
      let k = ListLabels.flatten ll in
      let _ = reachable.(Reachable.to_int cur_depth) <- k in
      let _ = loop graph k (Reachable.succ cur_depth) depth reachable in
      reachable
  in
  let reachable =
    Array.create ~len:(Reachable.to_int (Reachable.succ depth)) [start]
  in
  loop graph [start] depth k reachable

let%test_module _ =
  ( module struct

      (* simple sample data to make sure things generally work *)
      let v = [ ((Vertex.Label.of_int 10), [ Vertex.Edge.create (Vertex.Label.of_int 12)
                                           ; Vertex.Edge.create (Vertex.Label.of_int 15)
                                           ; Vertex.Edge.create (Vertex.Label.of_int 17) ])
              ; ((Vertex.Label.of_int 12), [ Vertex.Edge.create (Vertex.Label.of_int 5)
                                           ; Vertex.Edge.create (Vertex.Label.of_int 8)
                                           ; Vertex.Edge.create (Vertex.Label.of_int 10)
                                           ; Vertex.Edge.create (Vertex.Label.of_int 11) ])
              ; ((Vertex.Label.of_int 15), [ Vertex.Edge.create (Vertex.Label.of_int 10)
                                           ; Vertex.Edge.create (Vertex.Label.of_int 18)
                                           ; Vertex.Edge.create (Vertex.Label.of_int 24)
                                           ; Vertex.Edge.create (Vertex.Label.of_int 11) ])
              ; ((Vertex.Label.of_int 11), [ Vertex.Edge.create (Vertex.Label.of_int 2)
                                           ; Vertex.Edge.create (Vertex.Label.of_int 4)
                                           ; Vertex.Edge.create (Vertex.Label.of_int 12)
                                           ; Vertex.Edge.create (Vertex.Label.of_int 15) ])
              ]
              
    let graph =
      let s = S.create 25 in
      let _ = List.map v ~f:(fun (vert, edges) -> S.add_edges s vert edges) in
      s

    let%test "check_all_shortest_paths" =
      let start = Vertex.Label.of_int 10 in
      let target = Vertex.Label.of_int 11 in
      let depth = Reachable.of_int 3 in
      let paths = all_shortest_paths graph start target depth in
      Int.equal (List.length paths) 2

    let%test "check_k_reachable" =
      let kr =
        k_reachable graph (Vertex.Label.of_int 10) (Reachable.of_int 2)
      in
      Int.equal (Array.length kr) 3
  end )
