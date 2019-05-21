include Functor_intf

module Make (X : S) : S with type 'a t := 'a X.t = struct
  let map a func = X.map a func
end

module List = Make (struct
  type 'a t = 'a list

  let map a f = Base.List.map a ~f
end)

module Option = Make (struct
  type 'a t = 'a option

  let map o f = Base.Option.map o ~f
end)

module Result = Make (struct
  type 'a t = 'a Base.Or_error.t

  let map r f = Base.Or_error.map r ~f
end)

let%test "list functor" =
  let data = [1; 2; 3] in
  let results = List.map data (fun i -> i + 1) in
  Base.List.equal (fun i j -> Base.Int.equal i (j - 1)) data results

let%test "option functor" =
  let data = Some 10 in
  let results = Option.map data (fun i -> i + 1) in
  match (data, results) with Some 10, Some 11 -> true | _ -> false

let%test "result functor" =
  let first = Ok 100 in
  let second = Result.map first (fun i -> i + 100) in
  match second with Ok 200 -> true | _ -> false
