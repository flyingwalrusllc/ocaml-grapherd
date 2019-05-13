open Core

type t =
  | Empty
  | Simple of Label.t
  | Weighted of {id: Label.t; weight: Weight.t; }
  | Valid of {id: Label.t; weight: Weight.t; properties: Property.t list}
[@@deriving show, yojson]

let label e = match e with
  | Empty -> Label.empty
  | Simple l -> l
  | Weighted w -> w.id
  | Valid l -> l.id

let weight e =
  match e with
  | Empty -> Weight.empty
  | Simple _ ->  Weight.empty
  | Weighted w -> w.weight
  | Valid r -> r.weight

let properties e =
  match e with
  | Empty -> []
  | Simple _ -> []
  | Weighted _ -> []
  | Valid r -> r.properties

let create ?(weight = Weight.empty) ?(props = []) l =
  match (props, Weight.to_float weight) with
  | ([], Some _) -> Weighted {id= l; weight= weight}
  | ([], None) ->  Simple l
  | _ -> Valid { id= l; weight= weight; properties= props; }

let empty = Empty

let compare a b =
  let la = label a in
  let lb = label b in
  Label.compare la lb

let equal a b = Int.equal (compare a b) 0

let%test_module _ =
  ( module struct
    let ten = Label.of_int 10

    let eleven = Label.of_int 11

    let lbl10 = create ten

    let wght10 = create ten ~weight:(Weight.of_float 1.)

    let clr10 = create ten ~props:[Property.of_int 1]

    let lbl11 = create eleven

    let wght11 = create eleven ~weight:(Weight.of_float 1.)

    let clr11 = create eleven ~props:[Property.of_int 1]

    let%test "equal" =
      equal lbl10 wght10 && equal lbl10 clr10 && equal wght10 clr10
      && not (equal lbl11 wght10 && equal lbl10 clr11 && equal wght11 wght10)
  end )
