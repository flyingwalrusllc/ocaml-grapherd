open Core

module type Graph = sig
  type t

  type elt

  val create : int -> t
  (** [create i] create a graph with initial slot size of i *)

  val edges : t -> Vertex.Label.t -> Vertex.Edge.t list
  (** [egdes t label] return the list of edges on vertex label *)

  val label : t -> Vertex.Label.t -> Vertex.Label.t
  (** [label t label] get the actual label of the vertex at label

      this is only here for testing access
   *)

  val add_edge : t -> Vertex.Label.t -> Vertex.Edge.t -> unit

  val add_edges : t -> Vertex.Label.t -> Vertex.Edge.t list -> unit

  val get : t -> int -> elt

  val length : t -> int
end

module Make_graph (V : Vertex.Vertex) : Graph = struct
  type elt = V.t

  module DynArray = struct
    type t = {mutable ar: elt array; mutable n: int}

    let of_array ar =
      let n = Array.length ar in
      {ar; n}

    let get da i = da.ar.(i)

    let set da i v = da.ar.(i) <- v

    let length da = Array.length da.ar

  end

  type t = DynArray.t

  let init length start =
    let arr = Array.create ~len:length (V.create (Vertex.Label.of_int 0)) in
    let _ = Array.folding_mapi arr ~init:0 ~f:(fun i b _ ->
                let _ = Array.set arr i (V.create (Vertex.Label.of_int (i+start))) in
                (b, arr)) in
    arr

  let grow g id =
    let rec growth_amount current at_least =
      if current > at_least then current else growth_amount (current * 2) at_least in
    let length = DynArray.length g in
    let growth = (growth_amount length id) - length in
    if growth > length then
      g.ar <- Array.append g.ar (init growth length)

  let create i =
    let ar = init i 0 in
    DynArray.of_array ar
               
  let edges g l =
    let _ = grow g (Vertex.Label.to_int l) in
    V.edges (DynArray.get g (Vertex.Label.to_int l))

  let label g l = V.label (DynArray.get g (Vertex.Label.to_int l))

  let get g idx = DynArray.get g idx

  let add_edge g label edge =
    let idx = Vertex.Label.to_int label in
    let _ = grow g idx in
    let v = DynArray.get g idx in
    DynArray.set g idx (V.add_edge v edge )

  let add_edges g label edges =
    let idx = Vertex.Label.to_int label in
    let _ = grow g idx in
    let v = DynArray.get g idx in
    let _ = List.map edges ~f:(fun e -> V.add_edge v e) in
    ()

  let length g = DynArray.length g

end

let%test_module "list_vertex_graph" = (module struct

    module S = Make_graph(Vertex.Vertex_list)
             
    open Vertex.Label

    let%test "create" =
      let graph = S.create 100 in
      let es = S.edges graph (Vertex.Label.of_int 99) in
      Int.equal (List.length es) 0

    let%test "created_is_labeled" =
      let graph = S.create 100 in
      let rec check idx =
        let label = Vertex.Label.of_int idx in
        if (S.length graph) < idx then
          let v_label = S.label graph (Vertex.Label.of_int idx) in
          if label == v_label then check (idx+1) else
            false
        else true
      in
      check 0

    let%test "graph_grows" =
      let graph = S.create 10 in
      let _ = S.edges graph (Vertex.Label.of_int 20) in
      let len = S.length graph in
      Int.equal len 40

end)

let%test_module "set_vertex_graph" = (module struct

    module S = Make_graph(Vertex.Vertex_set)
             
    open Vertex.Label

    let%test "create" =
      let graph = S.create 100 in
      let es = S.edges graph (Vertex.Label.of_int 99) in
      Int.equal (List.length es) 0

    let%test "created_is_labeled" =
      let graph = S.create 100 in
      let rec check idx =
        let label = Vertex.Label.of_int idx in
        if (S.length graph) < idx then
          let v_label = S.label graph (Vertex.Label.of_int idx) in
          if label == v_label then check (idx+1) else
            false
        else true
      in
      check 0

    let%test "graph_grows" =
      let graph = S.create 10 in
      let _ = S.edges graph (Vertex.Label.of_int 20) in
      let len = S.length graph in
      Int.equal len 40

end)
