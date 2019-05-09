module type Vertex = sig
  type t

  val create : Label.t -> t

  val make : Label.t -> Edge.t list -> t

  val label : t -> Label.t

  val edge_count : t -> int

  val add_edge : t -> Edge.t -> t

  val remove_edge : t -> Edge.t -> t

  val edges : t -> Edge.t list

  val empty : t
end

module Vertex_list : Vertex = struct
  type t = {id: Label.t; mutable edges: Edge.t list}

  let create l = {id= l; edges= []}

  let make l edgs = {id= l; edges= edgs}

  let label v = v.id

  let edge_count v = List.length v.edges

  let add_edge v e =
    let n = e :: v.edges in
    v.edges <- n ;
    v

  let remove_edge v e =
    let n = List.filter (fun ee -> ee <> e) v.edges in
    v.edges <- n ;
    v

  let edges v = v.edges

  let empty = create (Label.of_int 0)

  let%test_module "Vertex_list" =
    ( module struct
      let edge12 = Edge.create (Label.of_int 12)

      let v = create (Label.of_int 10)

      let%test "create" =
        Pervasives.compare v {id= Label.of_int 10; edges= []} == 0

      let _ = add_edge v edge12

      let%test "add_edge" =
        Pervasives.compare v {id= Label.of_int 10; edges= [edge12]} == 0

      let%test "count_after_add" = edge_count v == 1

      let _ = remove_edge v edge12

      let%test "remove_edge" =
        Pervasives.compare v {id= Label.of_int 10; edges= []} == 0
    end )
end

module Vertex_set : Vertex = struct
  module EdgeSet = Set.Make (struct
    let compare = Pervasives.compare

    type t = Edge.t
  end)

  type t = {id: Label.t; edges: EdgeSet.t ref}

  let label v = v.id

  let create l = {id= l; edges= ref EdgeSet.empty}

  let make l edgs = {id= l; edges= ref (EdgeSet.of_list edgs)}

  let remove_edge v e =
    v.edges := EdgeSet.remove e !(v.edges) ;
    v

  let add_edge v e =
    let edges = EdgeSet.add e !(v.edges) in
    v.edges := edges ;
    v

  let edge_count v = List.length (EdgeSet.elements !(v.edges))

  let edges v = EdgeSet.elements !(v.edges)

  let empty = create (Label.of_int 0)

  let%test_module "Vertex_set" =
    ( module struct
      let edge12 = Edge.create (Label.of_int 12)

      let v = create (Label.of_int 10)

      let%test "create" =
        Pervasives.compare v {id= Label.of_int 10; edges= ref EdgeSet.empty}
        == 0

      let _ = add_edge v edge12

      let%test "add_edge" =
        Pervasives.compare v
          {id= Label.of_int 10; edges= ref (EdgeSet.singleton edge12)}
        == 0

      let%test "count_after_add" = edge_count v == 1

      let _ = remove_edge v edge12

      let%test "remove_edge" =
        Pervasives.compare v {id= Label.of_int 10; edges= ref EdgeSet.empty}
        == 0
    end )
end
