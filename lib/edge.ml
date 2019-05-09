type t =
  | Empty
  | Valid of {id: Label.t; weight: float; properties: Property.t list}
[@@deriving show]

let label e =
  match e with
  | Empty ->
      Label.empty
  | Valid l ->
      l.id

let weight e =
  match e with
  | Empty ->
      0.
  | Valid r ->
      r.weight

let properties e =
  match e with
  | Empty ->
      []
  | Valid r ->
      r.properties

let create ?(weight = 0.) ?(props = []) l =
  Valid { id= l; weight= weight; properties= props; }

let empty = Empty

let equal a b = match (a, b) with
  | (Empty, Empty) -> true
  | (Valid x, Valid y) -> Label.equal x.id y.id
  | (_, _) -> false

let%test_module _ =
  ( module struct
    let ten = Label.of_int 10

    let eleven = Label.of_int 11

    let lbl10 = create ten

    let lbl11 = create eleven

    let wght10 = create ten ~weight:1.

    let wght11 = create eleven ~weight:1.

    let clr10 = create ten ~props:[Property.of_int 1]

    let clr11 = create eleven ~props:[Property.of_int 1]

    let%test "equal" =
      equal lbl10 wght10 && equal lbl10 clr10 && equal wght10 clr10
      && not (equal lbl11 wght10 && equal lbl10 clr11 && equal wght11 wght10)
  end )
