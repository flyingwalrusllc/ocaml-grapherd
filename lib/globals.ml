
open Mysql_with_identity

let ( >>| ) x f =
  let open IO_result in
  x >>= fun x' -> return @@ f x'

let dbh = let handle = Mysql.quick_connect ~database:"test" ~user:"root" () in
          Prepared.init handle



