open Base
open Graph

include Api_intf

type 'a t = 'a list

let create a = [a; ]

let extend path a = List.cons a path
            
(* let pp_label_list ll =
 *   let _ = Stdio.print_string "\t[ " in
 *   let _ = List.map ll ~f:(fun l -> Caml.Printf.printf "%d; " (Label.T.to_int l)) in
 *   let _ = Stdio.print_endline "]" in
 *   () *)

(* let pp_label_list_array a =
 *   let _ = Stdio.print_endline "[| " in
 *   let _ = Array.map a ~f:(fun ll -> pp_label_list ll) in
 *   let _ = Stdio.print_endline " |]" in
 *   () *)
  

let exclude_visited children visited =
  List.filter children ~f:(fun l ->
      match Set.find visited ~f:(fun s -> Label.T.equal s l) with
      | Some _ ->
          false
      | None ->
          true )

let breadth_first_search graph start dest =
  let rec k_next graph vertices path_elements =
    List.fold vertices ~init:None ~f:(fun _ v ->
        if Label.T.equal v dest then
          Map.find path_elements v
        else
          match Map.find path_elements v with
          | None ->
             None
          | Some(path) ->
             let children = get graph v in
             let path = v :: path in
             let path_elements = Map.set path_elements ~key:v ~data:path in
             k_next graph children path_elements
      )
  in
  if Label.T.equal start dest then
    Some [ start; ]
  else
    k_next graph [start; ] (Map.empty (module Label))
                                 
  
let traverse_breadth_first graph ?(depth = 4) start =
  let rec next_gen graph nodes curr_depth visited =
    if curr_depth < depth then
      let level = nodes.(curr_depth - 1) in
      let children =
        List.fold level ~init:[] ~f:(fun acc n ->
            let children = get graph n in
            let children = exclude_visited children visited in
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
       let _ = visited := Set.add !visited start in
       (f start) :: (List.concat @@ List.map (get graph start) ~f:(fun next -> down graph next visited))
  in
  down graph start visited

  (* 
starting with current:

1. if current equals goal then return the path list for current
2. for each edge in current put current at the head of path for edge
3. call iter on the neighbors
   *)
let a_star graph start goal =
  let visited = ref (Set.empty (module Label)) in
  let paths = ref (Map.empty (module Label)) in
  let rec iter graph edges =
    let winners = List.filter edges ~f:(fun edge -> Label.T.equal edge goal) in
    let won = 0 < List.length winners in
    if won then
      Map.find !paths goal
    else
      let children =
        List.concat @@
          List.map edges ~f:(fun edge ->
              let _ = visited := Set.add !visited edge in
              let children = List.filter (Graph.get graph edge) ~f:(fun l -> not (Set.mem !visited l)) in
              let _ = List.map children ~f:(fun child ->
                          match Map.find !paths child with 
                            | Some path ->
                               paths := Map.set !paths ~key:child ~data:(List.cons edge path);
                            | None ->
                               paths := Map.set !paths ~key:child ~data:[edge; ];
                        )
              in
              children
            )
      in
      iter graph children
  in
  (*  iter graph [start; ] *)
  Option.map (iter graph [start; ]) ~f:(fun l ->
      let rec iter lst =
        match List.last lst with
        | Some cont ->
           begin
             match Map.find !paths cont with
             | Some lll -> iter (List.append lst lll)
             | None -> lst
           end
        | None -> lst
      in
      iter l)
    
      
let%test_module "test graph api" =
  ( module struct
    let graph =
      let g = Graph.create 100 in
      let _ = Graph.add g (Label.T.of_int 10) (Label.T.of_int 20) in
      let _ = Graph.add g (Label.T.of_int 10) (Label.T.of_int 11) in
      let _ = Graph.add g (Label.T.of_int 11) (Label.T.of_int 10) in
      let _ = Graph.add g (Label.T.of_int 20) (Label.T.of_int 10) in
      let _ = Graph.add g (Label.T.of_int 20) (Label.T.of_int 14) in
      let _ = Graph.add g (Label.T.of_int 14) (Label.T.of_int 20) in
      let _ = Graph.add g (Label.T.of_int 14) (Label.T.of_int 33) in
      let _ = Graph.add g (Label.T.of_int 33) (Label.T.of_int 14) in
      g

    let%test "test breadth first traversal" =
      let answer = traverse_breadth_first graph (Label.T.of_int 10) in
      let first = answer.(0) in
      (* let _ = pp_label_list_array answer in *)
      List.equal Label.T.equal first [Label.T.of_int 10]

    let%test "test depth first traversal" =
      let answer = traverse_depth_first graph (Label.T.of_int 10) ~f:(fun i -> i) in
      Int.equal (List.length answer) 5

    let%test "breadth first search" =
      let answer = breadth_first_search graph (Label.T.of_int 20) (Label.T.of_int 14) in
      match answer with
      | None ->
         let _ = Stdlib.Printf.printf "# None\n" in
         true
      | Some path ->
         let _ = Stdlib.Printf.printf "# %d\n" @@ List.length path in
         Int.equal (List.length path) 3

    let%test "a_star" =
      let answer = a_star graph (Label.T.of_int 10) (Label.T.of_int 20) in
      match answer with
      | Some path ->
         List.equal Label.T.equal path [(Label.T.of_int 10); ]
      | None -> false


    let%test "a_star" =
      match a_star graph (Label.T.of_int 10) (Label.T.of_int 33) with
      | Some path ->
         List.equal Label.T.equal path [(Label.T.of_int 14); (Label.T.of_int 20); (Label.T.of_int 10); ]

      | None -> false
      
  end )
