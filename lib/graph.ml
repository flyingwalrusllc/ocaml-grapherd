open Base
include Graph_intf

module Label = struct
  type t = int

  let of_int i = i

  let to_int l = l

  let equal a b = Int.equal (of_int a) (of_int b)
end

type 'a t = {mutable arr: 'a list array; mutable max: int}

let capacity t = Array.length t.arr

let set_capacity t cap =
  let addition = Array.create ~len:(cap - capacity t) [] in
  let updated = Array.concat [t.arr; addition] in
  Ok (t.arr <- updated)

let remove t l =
  try
    let _ = t.arr.(Label.to_int l) <- [] in
    Ok ()
  with Invalid_argument msg -> Error (Error.of_string msg)

let get t l = try t.arr.(Label.to_int l) with Invalid_argument _ -> []

let add t l a =
  try
    let idx = Label.to_int l in
    let curr = t.arr.(idx) in
    let update = a :: curr in
    Ok (t.arr.(idx) <- update)
  with Invalid_argument msg -> Error (Error.of_string msg)

let create initial_capacity =
  {arr= Array.create ~len:initial_capacity []; max= 0}

let%test_module "graph" =
  ( module struct
    let graph = create 100

    let l20 = Label.of_int 20

    let l30 = Label.of_int 30

    let l50 = Label.of_int 50

    let%test "add an edge" =
      match add graph l20 l50 with Ok _ -> true | Error _ -> false

    let%test "get edge back" =
      let edges = get graph l20 in
      let filtered = List.filter edges ~f:(fun edge -> Label.equal edge l50) in
      Int.equal (List.length filtered) 1

    let%test "add another edge and get two back" =
      let _ = add graph l20 l30 in
      let edges = get graph l20 in
      Int.equal (List.length edges) 2
  end )
