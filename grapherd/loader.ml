open Base
open Async
open Mysql_async

type link = {id: int32; user_id: int32; link_id: int32}

let link_of_tuple (id, user_id, link_id) = {id; user_id; link_id}

let get_max_user_id handle =
  let res =
    [%mysql select_one "select max(@int32{user_id}) from links"] handle
  in
  res

let get_all_links handle =
  let links =
    [%mysql
      select_all
        "SELECT @int32{id}, @int32{user_id}, @int32{link_id} FROM links"]
      handle
  in
  Result.map links ~f:(fun links ->
      List.map links ~f:(fun res -> link_of_tuple res) )

let get_links_for_user handle user_id =
  let stmt =
    [%mysql
      select_one
        "select @int32{id}, @int32{user_id}, @int32{link_id} from links where \
         user_id = %int32{user_id}"]
  in
  stmt handle ~user_id

let print_link link =
  let _ =
    Printf.printf "link: id=%ld user_id=%ld link_id=%ld \n" link.id
      link.user_id link.link_id
  in
  ()

let add_edge =
  [%mysql
    execute
      "insert into links (user_id, link_id) values (%int32{user_id}, \
       %int32{link_id})"]

let add_edges =
  [%mysql
    execute
      "insert into links (user_id, link_id) values \
       %list{(%int32{user_id},%int32{link_id})}"]

let remove_edge handle link =
  let stmt =
    [%mysql
      execute
        "delete from links where user_id=%int32{user_id} and \
         link_id=%int32{link_id}"]
  in
  stmt handle link.user_id link.link_id
