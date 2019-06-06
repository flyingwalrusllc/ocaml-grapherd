open Core
include Graph_intf

(** A simple wrapper around int that represents the id's of vertices *)
module Label = struct
  module T = struct
    type t = int [@@deriving sexp, compare, equal, hash]

    let of_int i = i

    let to_int l = l
                 
    let to_string l = Int.to_string (to_int l)

  end

  include T
  include Comparator.Make (T)
  
end

type 'a t = {arr: 'a list array; max: int} [@@deriving sexp]

let capacity graph = Array.length graph.arr

let set_capacity graph cap =
  let addition = Array.create ~len:(cap - capacity graph) [] in
  let updated = Array.concat [graph.arr; addition] in
  Ok {arr= updated; max= graph.max}

let clear graph label =
  let idx = Label.to_int label in
  let _ = graph.arr.(idx) <- [] in
  Ok graph

let remove graph label edge =
  try
    let idx = Label.to_int label in
    let edges = graph.arr.(idx) in
    let updated = List.filter edges ~f:(fun e -> Caml.compare e edge <> 0) in
    let _ = graph.arr.(idx) <- updated in
    Ok graph
  with Invalid_argument msg -> Error (Error.of_string msg)

let get t l = try t.arr.(Label.to_int l) with Invalid_argument _ -> []

let add graph label a =
  let freeboard = 1000 in
  let max_attempts = 3 in
  let rec add_int graph label a tries =
    let idx = Label.to_int label in
    if tries < max_attempts then
      match
        try
          let curr = graph.arr.(idx) in
          let update = a :: curr in
          let _ = graph.arr.(idx) <- update in
          Ok graph
        with Invalid_argument _ -> (
          (* if the access was out of bounds then resize the internall array *)
          match set_capacity graph (idx + freeboard) with
          | Ok g ->
              add_int g label a (tries + 1)
          | Error _ ->
              Error (Error.of_string "unable to resize graph") )
      with
      | Ok z ->
          Ok z
      | Error _ ->
          add_int graph label a (tries + 1)
    else Error (Error.of_string "To many attempts")
  in
  add_int graph label a 0

let create initial_capacity =
  {arr= Array.create ~len:initial_capacity []; max= 0}

let%test_module "graph" =
  ( module struct
    let initial_graph = create 100

    let l20 = Label.of_int 20

    let l30 = Label.of_int 30

    let l50 = Label.of_int 50

    let l800 = Label.of_int 800

    let l1000 = Label.of_int 1000

    let extract_or_raise graph_res msg =
      match graph_res with
      | Ok g ->
          g
      | Error _ ->
          raise (Invalid_argument msg)

    let graph =
      extract_or_raise (add initial_graph l20 l50) "add of l50 failed"

    let%test "add an edge" =
      let edges = get graph l20 in
      Int.equal (List.length edges) 1

    let%test "get edge back" =
      let edges = get graph l20 in
      let filtered = List.filter edges ~f:(fun edge -> Label.equal edge l50) in
      Int.equal (List.length filtered) 1

    let graph = extract_or_raise (add graph l20 l30) "add of l30 failed"

    let%test "add another edge and get two back" =
      let edges = get graph l20 in
      Int.equal (List.length edges) 2

    let graph =
      extract_or_raise (add graph l1000 l800) "add of l800 to l1000 failed"

    let%test "increase capacity" = capacity graph > 1000
  end )
