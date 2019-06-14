open Base
open Graph
include Api_intf

type 'a t = Label.t list

[@@@ocaml.warning "-32"]

let pp_label_list ll =
  let _ = Stdio.print_string "\t[ " in
  let _ =
    List.map ll ~f:(fun l -> Caml.Printf.printf "%d; " (Label.T.to_int l))
  in
  let _ = Stdio.print_endline "]" in
  ()

let pp_label_list_array a =
  let _ = Stdio.print_endline "[| " in
  let _ = Array.map a ~f:(fun ll -> pp_label_list ll) in
  let _ = Stdio.print_endline " |]" in
  ()

[@@@end]

let create a = [a]

let extend path a = List.cons a path

let exclude_visited children visited =
  List.filter children ~f:(fun l ->
      match Set.find visited ~f:(fun s -> Label.T.equal s l) with
      | Some _ ->
          false
      | None ->
          true )

let rec build_path comes_from path label =
  let _ = pp_label_list path in
  match Hashtbl.find comes_from label with
  | Some previous_v ->
      build_path comes_from (previous_v :: path) previous_v
  | None ->
      List.rev path

let a_star graph start dest =
  let visited = ref (Set.empty (module Label)) in
  let queue = Linked_queue.create () in
  let came_from = Hashtbl.create (module Label.T) in
  let rec iter () =
    match Linked_queue.dequeue queue with
    | Some c ->
        if Label.T.equal c dest then Some c
        else
          let _ = visited := Set.add !visited c in
          let children = Graph.get graph c in
          let _ =
            List.iter children ~f:(fun child ->
                if Set.mem !visited child then ()
                else
                  let _ = Hashtbl.set came_from ~key:child ~data:c in
                  let _ = Linked_queue.enqueue queue child in
                  () )
          in
          iter ()
    | None ->
        None
  in
  if Label.T.equal start dest then [start]
  else
    let _ = Linked_queue.enqueue queue start in
    match iter () with Some _ -> build_path came_from [] dest | None -> []

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

    let%test "a_star" =
      let path = a_star graph (Label.T.of_int 10) (Label.T.of_int 20) in
      let _ = pp_label_list path in
      List.equal Label.T.equal path [Label.T.of_int 10]

    let%test "a_star" =
      let path = a_star graph (Label.T.of_int 10) (Label.T.of_int 33) in
      let _ = pp_label_list path in
      List.equal Label.T.equal path
        [Label.T.of_int 14; Label.T.of_int 20; Label.T.of_int 10]
  end )
