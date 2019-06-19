open Base
include Vertex_intf

type label = int [@@deriving compare, equal, sexp]

module Edge = struct
  module T = struct
    type t =
      {id: label; weight: float; mutable properties: (string * string) list}
    [@@deriving compare, equal, sexp, fields]
  end

  let empty : T.t = {id= 0; weight= 0.; properties= []}
end

type t = {id: label; weight: float; edge_count: int; edges: Edge.T.t array}
[@@deriving sexp]

let create ?(weight = 1.) ?(edges = []) label =
  let edge_count = List.length edges in
  let edge_array =
    if edge_count < 20 then
      let arr = Array.create ~len:20 Edge.empty in
      let _ = List.iteri edges ~f:(fun idx edge -> arr.(idx) <- edge) in
      arr
    else List.to_array edges
  in
  {id= label; weight; edge_count; edges= edge_array}

let id vert = vert.id

let edges vert = Array.to_list vert.edges
