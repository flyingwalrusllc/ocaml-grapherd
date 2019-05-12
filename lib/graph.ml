open Core

module type Graph = sig
  type t

  type elt

  val create : int -> t
  (** [create i] create a graph with initial slot size of i *)

  val edges : t -> Label.t -> Edge.t list
  (** [egdes t label] return the list of edges on vertex label *)

  val edge_labels : t -> Label.t -> Label.t list Or_error.t
  (** [edge_labels t l] list of labels that are the id's for the outgoing edges of the vertex l *)
 
  val label : t -> Label.t -> Label.t
  (** [label t label] get the actual label of the vertex at label

      this is only here for testing access
   *)

  val remove_edge : t -> Label.t -> Label.t -> unit Or_error.t
  (** [remove_edge t l] remove from vertex l the edge *)

  val add_edge : t -> Label.t -> Edge.t -> unit Or_error.t

  val add_edges : t -> Label.t -> Edge.t list -> unit Or_error.t

  val length : t -> int
end

module Make_graph (V : Vertex.Vertex) : Graph = struct
  type elt = V.t

  (* the problem with this is that there's no control around of
     concurrency. there seem to be no atomic operations available
     directly in ocaml so I suppose the only way forward is some
     locking method. Even though this looks public in the end this
     Functor returns a graph which exposes nothing about this. *)
  module GrowableArray = struct
    (* array's are mutable by their nature but in this case we need
       the wrapper to have a mutable reference to the array so we can
       grow it when needed. The n in this case is the upper limit of
       how many slots in the array contain actual data rather then the
       size of the array. *)
    type t = {mutable ar: elt array; mutable n: int}

    let get da i = da.ar.(i)

    let set da i v = da.ar.(i) <- v

    let length da = Array.length da.ar

    let growable_mutex = Nano_mutex.create ()

    let rec size_to_accomidate current needed =
      if current > needed then current
      else size_to_accomidate (current * 2) needed

    let resize ga arr =
      let g =
        Nano_mutex.critical_section growable_mutex ~f:(fun _ ->
            let ar1 = Array.append ga.ar arr in
            ga.ar <- ar1 ;
            ga )
      in
      g

    let of_array ar =
      let n = Array.length ar in
      {ar; n}
  end

  type t = GrowableArray.t

  (* initialize an array. this is only here because each slot has it's
     label as well as it's edges. this could easily be removed and at
     some point and the wrapper type could be returned as part the a
     graph get without being stored. *)
  let init length start =
    let arr = Array.create ~len:length (V.create Label.empty) in
    let _ =
      Array.folding_mapi arr ~init:0 ~f:(fun i b _ ->
          let _ = arr.(i) <- V.create (Label.of_int (i + start)) in
          (b, arr) )
    in
    arr

  (* this seem more complicated than it needs to be. if the graph
     doesn't have sufficient slots then grow the underlying GrowableArray
     to have a slot for the referenced label. *)
  let grow g id =
    let length = GrowableArray.length g in
    let needed = GrowableArray.size_to_accomidate length id in
    let additional = needed - length in
    if additional > 0 then GrowableArray.resize g (init additional length)
    else g

  let create i =
    let ar = init i 0 in
    GrowableArray.of_array ar

  let invalid_vertex = Error.of_string "Invalid vertex"
                     
  let edges g l =
    match Label.to_int l with
    | Some x ->
        let _ = grow g x in
        V.edges (GrowableArray.get g x)
    | None ->
        []

  let edge_labels graph label =
    if Label.equal label Label.empty then Error invalid_vertex
    else
      Ok
        (List.map (edges graph label) ~f:(fun e -> Edge.label e))

  let label g l =
    match Label.to_int l with
    | Some x ->
        V.label (GrowableArray.get g x)
    | None ->
        Label.empty
                     
  let add_edge g label edge =
    match Label.to_int label with
    | Some idx ->
        let _ = grow g idx in
        let v = GrowableArray.get g idx in
        let _ = GrowableArray.set g idx (V.add_edge v edge) in
        Ok ()
    | None ->
        Error invalid_vertex

  let add_edges g label edges =
    match Label.to_int label with
    | Some idx ->
        let _ = grow g idx in
        let v = GrowableArray.get g idx in
        let _ = List.map edges ~f:(fun e -> V.add_edge v e) in
        Ok ()
    | None ->
       Error invalid_vertex

  let remove_edge graph vert edg =
    match Label.to_int vert with
    | Some idx ->
       let updated = List.filter (edges graph vert) ~f:(fun e -> not (Label.equal edg (Edge.label e))) in
        let _ = grow graph idx in
        let _ = GrowableArray.set graph idx (V.create vert ~edges:updated) in
        Ok ()
    | None ->
        Error (Error.of_string "Invalid vertex")

  let length g = GrowableArray.length g
end

let%test_module "list_vertex_graph" =
  ( module struct
    module S = Make_graph (Vertex.Vertex_list)

    let%test "create" =
      let graph = S.create 100 in
      let es = S.edges graph (Label.of_int 99) in
      Int.equal (List.length es) 0

    let%test "all graph slots have labels" =
      let graph = S.create 100 in
      let rec check idx =
        let label = Label.of_int idx in
        if S.length graph < idx then
          let v_label = S.label graph (Label.of_int idx) in
          if Label.equal label v_label then check (idx + 1) else false
        else true
      in
      check 0

    let%test "grow graph" =
      let graph = S.create 10 in
      let _ = S.edges graph (Label.of_int 20) in
      let len = S.length graph in
      Int.equal len 40
  end )

let%test_module "set_vertex_graph" =
  ( module struct
    module S = Make_graph (Vertex.Vertex_set)

    let%test "create" =
      let graph = S.create 100 in
      let es = S.edges graph (Label.of_int 99) in
      Int.equal (List.length es) 0

    let%test "created_is_labeled" =
      let graph = S.create 100 in
      let rec check idx =
        let label = Label.of_int idx in
        if S.length graph < idx then
          let v_label = S.label graph (Label.of_int idx) in
          if Label.equal label v_label then check (idx + 1) else false
        else true
      in
      check 0

    let%test "graph_grows" =
      let graph = S.create 10 in
      let _ = S.edges graph (Label.of_int 20) in
      let len = S.length graph in
      Int.equal len 40
  end )
