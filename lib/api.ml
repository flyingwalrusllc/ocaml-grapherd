open Base
open Graph

include Api_intf

let filter_children children visited =
  List.filter children ~f:(fun l ->
      match Set.find visited ~f:(fun s -> Label.T.equal s l) with
      | Some _ ->
          false
      | None ->
          true )

let traverse_breadth_first graph ?(depth = 4) start =
  let rec next_gen graph nodes curr_depth visited =
    if curr_depth < depth then
      let level = nodes.(curr_depth - 1) in
      let children =
        List.fold level ~init:[] ~f:(fun acc n ->
            let children = get graph n in
            let children = filter_children children visited in
            List.append acc children )
      in
      let children = List.dedup_and_sort ~compare:Label.T.compare children in
      let childs = Set.of_list (module Label) children in
      let _ = nodes.(curr_depth) <- children in
      next_gen graph nodes (curr_depth + 1) (Set.union visited childs)
    else nodes
  in
  let retv = Array.create ~len:(depth + 1) [] in
  let _ = retv.(0) <- [start] in
  let visited = Set.of_list (module Label) [start] in
  next_gen graph retv 1 visited

let traverse_depth_first graph ~(f : 'a -> 'b) start =
  let visited = ref (Set.of_list (module Label) []) in
  let rec down graph start visited =
    match Set.find !visited ~f:(fun s -> Label.T.equal start s) with
    | Some _ -> []
    | None ->
       let _ = Stdlib.Printf.printf " down %d\n" (Label.T.to_int start) in
       let _ = visited := Set.add !visited start in
       (f start) :: (List.concat @@ List.map (get graph start) ~f:(fun next -> down graph next visited))
  in
  down graph start visited
                         
let pp_label_list ll =
  let _ = Stdio.print_string "\t[ " in
  let _ = List.map ll ~f:(fun l -> Caml.Printf.printf "%d; " (Label.T.to_int l)) in
  let _ = Stdio.print_endline " ]" in
  ()

let pp_label_list_array a =
  let _ = Stdio.print_endline "[| " in
  let _ = Array.map a ~f:(fun ll -> pp_label_list ll) in
  let _ = Stdio.print_endline " |]" in
  ()
  
let%test_module "test graph api" =
  ( module struct
    let graph =
      let g = Graph.create 100 in
      let _ = Graph.add g (Label.T.of_int 10) (Label.T.of_int 20) in
      let _ = Graph.add g (Label.T.of_int 20) (Label.T.of_int 10) in
      let _ = Graph.add g (Label.T.of_int 10) (Label.T.of_int 11) in
      let _ = Graph.add g (Label.T.of_int 11) (Label.T.of_int 10) in
      let _ = Graph.add g (Label.T.of_int 20) (Label.T.of_int 14) in
      let _ = Graph.add g (Label.T.of_int 14) (Label.T.of_int 20) in
      let _ = Graph.add g (Label.T.of_int 14) (Label.T.of_int 10) in
      let _ = Graph.add g (Label.T.of_int 14) (Label.T.of_int 11) in
      g

    let%test "test breadth first traversal" =
      let answer = traverse_breadth_first graph (Label.T.of_int 10) in
      let first = answer.(0) in
      let _ = pp_label_list_array answer in
      List.equal Label.T.equal first [Label.T.of_int 10]

    let%test "test depth first traversal" =
      let answer = traverse_depth_first graph (Label.T.of_int 10) ~f:(fun i -> i) in
      let _ = pp_label_list answer in
      true
      
  end )
