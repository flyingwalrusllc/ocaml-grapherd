(* open Async
 * open Core *)

let dummy _ = "Hello world"

let%test _ =
  let msg = dummy () in
  String.equal msg "Hello world"
