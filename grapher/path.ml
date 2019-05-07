open Core

type t = Vertex.Label.t list [@@deriving show]

let append path0 label = path0 @ [label]

let empty label = [label]

let length path = List.length path - 1

let print path =
  let _ = Printf.printf "\nPath length %d " (List.length path) in
  let _ =
    List.map path ~f:(fun l -> Printf.printf "%d -> " (Vertex.Label.to_int l))
  in
  let _ = print_endline ";" in
  ()

let print_path_list path_list =
  let _ = Printf.printf "Path list length %d:\n" (List.length path_list) in
  let _ = List.map path_list ~f:(fun p -> show p) in
  ()
