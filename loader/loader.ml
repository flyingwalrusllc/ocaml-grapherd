open Mysql_with_identity

type link =
  { id : int32;
    user_id : int32;
    link_id : int32 }

let link_of_tuple (id, user_id, link_id) =
  { id= id;
    user_id= user_id;
    link_id= link_id }

let ( >>| ) x f =
  let open IO_result in
  x >>= fun x' -> return @@ f x'

let get_all_links handle =
  [%mysql select_all "SELECT @int32{id}, @int32{user_id}, @int32{link_id} FROM links"] handle
  >>| List.map link_of_tuple

let print_link link =
  let _ = Printf.printf "link: id=%ld user_id=%ld link_id=%ld \n" link.id link.user_id link.link_id in
  ()
  
let test handle =
  let open IO_result in
  get_all_links handle
  >>=
    fun all_links ->
    let _ = Printf.printf "all users: \n" in
    let _ = List.iter print_link all_links in
    Ok ()
    

let main _ =
  let connection = Mysql.quick_connect ~database:"caml" ~user:"ocaml" ~password:"functional" () in
  let handle = Prepared.init connection in
  let res = test handle in
  Mysql.disconnect connection;
  match res with
  | Ok () -> Printf.printf "All went well!\n"
  | Error _ -> Printf.printf "An error occurred!\n"

let _ = main ()
