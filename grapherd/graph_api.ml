open Core
   
(* let max_k = 4 *)

let all_shortest_paths _ _ _ _ = Ok []

let k_reachable _ _ _ = Array.create ~len:1 []

(* let%test_module _ =
 *   ( module struct
 *     (\* simple sample data to make sure things generally work *\)
 *     let v =
 *       [ ( Label.of_int 10
 *         , [ Edge.create (Label.of_int 12)
 *           ; Edge.create (Label.of_int 15)
 *           ; Edge.create (Label.of_int 17) ] )
 *       ; ( Label.of_int 12
 *         , [ Edge.create (Label.of_int 5)
 *           ; Edge.create (Label.of_int 8)
 *           ; Edge.create (Label.of_int 10)
 *           ; Edge.create (Label.of_int 11) ] )
 *       ; ( Label.of_int 15
 *         , [ Edge.create (Label.of_int 10)
 *           ; Edge.create (Label.of_int 18)
 *           ; Edge.create (Label.of_int 24)
 *           ; Edge.create (Label.of_int 11) ] )
 *       ; ( Label.of_int 11
 *         , [ Edge.create (Label.of_int 2)
 *           ; Edge.create (Label.of_int 4)
 *           ; Edge.create (Label.of_int 12)
 *           ; Edge.create (Label.of_int 15) ] ) ]
 * 
 *     let graph =
 *       let s = S.create 25 in
 *       let _ = List.map v ~f:(fun (vert, edges) -> S.add_edges s vert edges) in
 *       s
 * 
 *     let%test "check_all_shortest_paths" =
 *       let _ = print_endline "## check_all_shortest_paths\n" in
 *       let start = Label.of_int 10 in
 *       let target = Label.of_int 11 in
 *       let depth = Reachable.of_int 3 in
 *       let paths = all_shortest_paths graph start target depth in
 *       match paths with
 *       | Ok p -> Int.equal (List.length p) 0 (\* 2 *\)
 *       | Error _ -> false
 * 
 *     let%test "check_k_reachable" =
 *       let _ = print_endline "## check_k_reachable\n" in
 *       let kr = k_reachable graph (Label.of_int 10) (Reachable.of_int 2) in
 *       Int.equal (Array.length kr) 3
 *   end ) *)
