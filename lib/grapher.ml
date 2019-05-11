open Core
module S = Graph.Make_graph (Vertex.Vertex_list)

let max_k = Reachable.of_int 4

let all_shortest_paths graph start target distance =
  let limit =
    if Reachable.compare distance max_k > 0 then max_k else distance
  in
  let rec loop candidate path found =
    let labels = S.edge_labels graph candidate in
    let depth = Reachable.of_int (Path.length path) in
    if Reachable.compare depth limit > 0 then found
    else
      List.concat
        (List.map labels ~f:(fun l ->
             if not (Label.equal l start) then
               let found_path = Path.append path l in
               if Label.equal l target then
                 let ff = found_path :: found in
                 ff
               else
                 let fl = loop l found_path found in
                 fl
             else found ))
  in
  loop start (Path.empty start) []

let k_reachable graph start k =
  let depth = if Reachable.compare k max_k > 0 then max_k else k in
  let reach_n graph labels =
    List.fold labels ~init:[] ~f:(fun acc label ->
        List.append (S.edge_labels graph label) acc )
  in
  let rec loop graph labels cur_depth depth reachable =
    if Reachable.compare cur_depth depth >= 0 then reachable
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
    let v =
      [ ( Label.of_int 10
        , [ Edge.create (Label.of_int 12)
          ; Edge.create (Label.of_int 15)
          ; Edge.create (Label.of_int 17) ] )
      ; ( Label.of_int 12
        , [ Edge.create (Label.of_int 5)
          ; Edge.create (Label.of_int 8)
          ; Edge.create (Label.of_int 10)
          ; Edge.create (Label.of_int 11) ] )
      ; ( Label.of_int 15
        , [ Edge.create (Label.of_int 10)
          ; Edge.create (Label.of_int 18)
          ; Edge.create (Label.of_int 24)
          ; Edge.create (Label.of_int 11) ] )
      ; ( Label.of_int 11
        , [ Edge.create (Label.of_int 2)
          ; Edge.create (Label.of_int 4)
          ; Edge.create (Label.of_int 12)
          ; Edge.create (Label.of_int 15) ] ) ]

    let graph =
      let s = S.create 25 in
      let _ = List.map v ~f:(fun (vert, edges) -> S.add_edges s vert edges) in
      s

    let%test "check_all_shortest_paths" =
      let start = Label.of_int 10 in
      let target = Label.of_int 11 in
      let depth = Reachable.of_int 3 in
      let paths = all_shortest_paths graph start target depth in
      Int.equal (List.length paths) 2

    let%test "check_k_reachable" =
      let kr = k_reachable graph (Label.of_int 10) (Reachable.of_int 2) in
      Int.equal (Array.length kr) 3
  end )
