open Core

type t = Label.t list [@@deriving show]

let append path0 label = path0 @ [label]

let empty label = [label]

let length path = List.length path - 1
