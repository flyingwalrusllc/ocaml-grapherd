include Applicative_intf

module Make (X : S) : S with type 'a t := 'a X.t = struct
  let pure a = X.pure a

  let ap a func = X.ap a func

  let map a func = ap a (pure func)
end

module List = Make (struct
  type 'a t = 'a list

  let pure a = [a]

  let ap a func =
    Base.List.concat (Base.List.map func ~f:(fun f -> Base.List.map a ~f))

  (* let ap (a : 'a list) (func : ('a -> 'b) list) =
                   *   Base.List.concat
                   *     (Base.List.map func ~f:(fun f -> Base.List.map a ~f:f)) *)

  let map a func = ap a (pure func)
end)

module Option = Make (struct
  type 'a t = 'a option

  let pure a = Some a

  let ap (a : 'a option) (func : ('a -> 'b) option) =
    match (a, func) with Some aa, Some f -> Some (f aa) | _ -> None

  let map a func = ap a (pure func)
end)

let%test "list applicative" =
  let a = [32] in
  let f = [(fun i -> i + 1)] in
  let ap_ap = List.ap a f in
  Base.Int.equal (Base.List.length ap_ap) 1

let%test "option applicative" =
  let a = Some "bob" in
  let f = Some (fun i -> Printf.sprintf "Hello %s" i) in
  let ap_ap = Option.ap a f in
  match ap_ap with
  | Some str ->
      Base.Int.equal (String.length str) 9
  | _ ->
      false
