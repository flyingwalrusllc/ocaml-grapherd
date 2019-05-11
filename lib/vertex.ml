open Core

module type Vertex = sig
  type t [@@deriving show, yojson]

  val create : ?edges:Edge.t list -> Label.t -> t

  val label : t -> Label.t

  val edge_count : t -> int

  val add_edge : t -> Edge.t -> t

  val add_edges : t -> Edge.t list -> t

  val remove_edge : t -> Label.t -> t

  val edges : t -> Edge.t list

  val empty : t
end

module Vertex_list : Vertex = struct
  type t = {id: Label.t; mutable edges: Edge.t list} [@@deriving show, yojson]

  let create ?(edges = []) l = {id= l; edges}

  let label v = v.id

  let edges v = v.edges

  let edge_count v = List.length v.edges

  let add_edges v le =
    let new_edges = List.append le v.edges in
    let _ = v.edges <- new_edges in
    v

  let add_edge v e = add_edges v [e]

  let remove_edge v l =
    let edges = edges v in
    let n =
      List.filter edges ~f:(fun edge -> not (Label.equal l (Edge.label edge)))
    in
    v.edges <- n ;
    v

  let empty = create (Label.of_int 0)

  let%test_module "Vertex_list" =
    ( module struct
      let label10 = Label.of_int 10

      let label12 = Label.of_int 12

      let label08 = Label.of_int 8

      let edge08 = Edge.create label08

      let edge12 = Edge.create label12

      let v = create label10

      let%test "create" = Label.equal (label v) label10

      let _ = add_edge v edge12

      let%test "add_edge" =
        let should_be = create ~edges:[edge12] label10 in
        Int.equal (Pervasives.compare v should_be) 0

      let%test "count_after_add" = Int.equal (edge_count v) 1

      let%test "remove_edge" =
        let _ = remove_edge v label12 in
        Int.equal (edge_count v) 0

      let%test "add_another_edge" =
        let _ = add_edges v [edge08; edge12] in
        let should = create ~edges:[edge08; edge12] label10 in
        Int.equal (Pervasives.compare v should) 0
    end )
end

module Vertex_set : Vertex = struct
  module EdgeSet = Caml.Set.Make (struct
    let compare = Caml.compare

    type t = Edge.t
  end)

  type t = {id: Label.t; edges: EdgeSet.t ref}

  let of_yojson (json : Yojson.Safe.t) : (t, string) result =
    let open Core in
    let t_opt =
      match json with
      | `Assoc alist ->
          let id_opt = List.Assoc.find ~equal:String.equal alist "id" in
          let edges_opt = List.Assoc.find alist ~equal:String.equal "edges" in
          Option.map2 id_opt edges_opt ~f:(fun l e ->
              let id =
                match Label.of_yojson l with
                | Ok lbl ->
                    lbl
                | Error _ ->
                    Label.empty
              in
              let edges =
                match e with
                | `List le ->
                    List.map le ~f:(fun j ->
                        match Edge.of_yojson j with
                        | Ok edg ->
                            edg
                        | Error _ ->
                            Edge.empty )
                | _ ->
                    []
              in
              {id; edges= ref (EdgeSet.of_list edges)} )
      | _ ->
          None
    in
    Result.of_option t_opt ~error:"foobar"

  let to_yojson t =
    let open Core in
    `Assoc
      [ ("id", Label.to_yojson t.id)
      ; ( "edges"
        , `List
            (List.map (EdgeSet.elements !(t.edges)) ~f:(fun elt ->
                 Edge.to_yojson elt )) ) ]

  let pp _ _ = ()

  let show _ = ""

  let label v = v.id

  let create ?(edges = []) l = {id= l; edges= ref (EdgeSet.of_list edges)}

  let remove_edge v l =
    v.edges := EdgeSet.remove (Edge.create l) !(v.edges) ;
    v

  let add_edge v e =
    let edges = EdgeSet.add e !(v.edges) in
    v.edges := edges ;
    v

  let add_edges v le =
    let edges = EdgeSet.union (EdgeSet.of_list le) !(v.edges) in
    v.edges := edges ;
    v

  let edge_count v = List.length (EdgeSet.elements !(v.edges))

  let edges v = EdgeSet.elements !(v.edges)

  let empty = create (Label.of_int 0)

  let%test_module "Vertex_set" =
    ( module struct
      let label10 = Label.of_int 10

      let label12 = Label.of_int 12

      let label08 = Label.of_int 8

      let edge08 = Edge.create label08

      let edge12 = Edge.create label12

      let v = create label10

      let%test "create" = Label.equal (label v) label10

      let _ = add_edge v edge12

      let%test "add_edge" =
        let should_be = create ~edges:[edge12] label10 in
        Int.equal (Pervasives.compare v should_be) 0

      let%test "count_after_add" = Int.equal (edge_count v) 1

      let%test "remove_edge" =
        let _ = remove_edge v label12 in
        Int.equal (edge_count v) 0

      let%test "add_another_edge" =
        let _ = add_edges v [edge08; edge12] in
        let should = create ~edges:[edge08; edge12] label10 in
        Int.equal (Pervasives.compare v should) 0

      let%test "remove_edge" =
        let _ = remove_edge v label12 in
        let should = create label10 ~edges:[edge08] in
        Int.equal (Pervasives.compare v should) 0

      (* this is an extra test, the other version(s) of vertex have
         derived (code generated) versions of this. because set isnt'
         deriving yojson friendly the to_ and of_ methods are hand
         writen. *)
      let%test "json serialization round trip" =
        let open Core in
        let str = Yojson.Safe.to_string (to_yojson v) in
        let round = of_yojson (Yojson.Safe.from_string str) in
        match round with
        | Ok good ->
            let pairs = List.zip (edges v) (edges good) in
            let p =
              match pairs with
              | Some pp ->
                  List.fold_left pp ~init:false ~f:(fun _ p ->
                      match p with a, b -> Edge.equal a b )
              | None ->
                  false
            in
            Label.equal (label v) (label good) && p
        | Error _ ->
            false
    end )
end
