open Base

type t = {id: int; name: string; count: int} [@@deriving compare]

let tag_cloud = Hashtbl.create (module String)

let from_name name = Hashtbl.find tag_cloud name

let name tag = tag.name

let equal a b = Int.equal (compare a b) 0

let greater_than a b = compare a b > 0

let greater_than_or_equal a b = compare a b >= 0

let less_than a b = compare a b < 0

let less_than_or_equal a b = compare a b <= 0

let update_count ?(incr = 1) name =
  match Hashtbl.find tag_cloud name with
  | Some tag ->
      let updated = {id= tag.id; name= tag.name; count= tag.count + incr} in
      let _ = Hashtbl.set tag_cloud ~key:name ~data:updated in
      Ok ()
  | None ->
      Error (Error.of_string "No such tag")

let create id name count =
  let tag = {id; name; count} in
  let foo = Hashtbl.add tag_cloud ~key:name ~data:tag in
  match foo with
  | `Ok ->
      tag
  | `Duplicate -> (
    match Hashtbl.find tag_cloud name with
    | Some current ->
        let updated = {id; name; count= current.count + 1} in
        let _ = Hashtbl.set tag_cloud ~key:name ~data:updated in
        updated
    | None ->
        tag )

let%test "create tag" =
  let tag = create 1 "tag1" 1 in
  equal tag {id= 1; name= "tag1"; count= 1}

let%test "create a different tag" =
  let tag = create 1 "tag2" 1 in
  equal tag {id= 1; name= "tag2"; count= 1}

let%test "tags get saved in cloud" =
  match from_name "tag1" with
  | Some tag ->
      equal tag {id= 1; name= "tag1"; count= 1}
  | None ->
      false

let%test "update count default incr" =
  let func_ret = match update_count "tag1" with Ok _ -> true | _ -> false in
  match from_name "tag1" with
  | Some tag ->
      func_ret && equal tag {id= 1; name= "tag1"; count= 2}
  | None ->
      false

let%test "update count with explicit incr" =
  let func_ret =
    match update_count "tag1" ~incr:5 with Ok _ -> true | _ -> false
  in
  match from_name "tag1" with
  | Some tag ->
      func_ret && equal tag {id= 1; name= "tag1"; count= 7}
  | None ->
      false

let%test "counts are updated on duplicate create" =
  let tag = create 1 "tag1" 1 in
  equal tag {id= 1; name= "tag1"; count= 8}

let%test "unreferenced tags don't get updated" =
  match from_name "tag2" with
  | Some tag ->
      equal tag {id= 1; name= "tag2"; count= 1}
  | None ->
      false
