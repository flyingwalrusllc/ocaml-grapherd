open Core

type t = {id: int; tag: string; count: int}

let tag_cloud = Hashtbl.create (module String)

let from_string name =
  match Hashtbl.find tag_cloud name with
  | Some tag ->
      tag
  | None ->
      let idx =
        Hashtbl.fold tag_cloud ~init:0 ~f:(fun ~key:_ ~data idx ->
            if data.id > idx then data.id else idx )
      in
      let tag = {id= idx + 1; tag= name; count= 1} in
      let _ = Hashtbl.add tag_cloud ~key:name ~data:tag in
      tag

let to_string tag = tag.tag

let compare a b = Int.compare a.id b.id

let equal a b = phys_equal (compare a b) 0

let greater_than a b = compare a b > 0

let greater_than_or_equal a b = compare a b >= 0

let less_than a b = compare a b < 0

let less_than_or_equal a b = compare a b <= 0
