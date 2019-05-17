open Core

type 'a t =
  { mutable arr: 'a array
  ; zero: 'a
  ; mutable max: int }

let of_array ar zero =
  { arr= Array.copy ar
  ; zero= zero
  ; max= 0 }

let get da i =
  Array.get da.arr i 

let set da i v = Array.set da.arr i v

let max da = da.max

let grow g id =
  let rec calc_growth current at_least =
    if current > at_least then current
    else calc_growth (current * 2) at_least
  in
  let n_size = calc_growth (Array.length g.arr) id in
  let growth = n_size - (Array.length g.arr) in
  if growth > 0 then
    let n_arr = Array.create ~len:growth g.zero in
    let _ = g.arr <- Array.append g.arr n_arr in
    g
  else g

let%test_module _ = (module struct
                       let start = of_array (Array.create ~len:100 0) 0

                       let%test "get existing" =
                         let x = get start 50 in
                         Int.equal x 0

                       let%test "set existing" =
                         let _ = set start 50 50 in
                         let x = get start 50 in
                         Int.equal x 50

                       let%test "grow" =
                         let _ = grow start 1000 in
                         let _ = set start 666 23 in
                         let x = get start 666 in
                         Int.equal x 23
                         
                       end)
