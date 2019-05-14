open Base

include Graph_intf

module Label = struct
  type t = int

  let of_int i = i
  let to_int l = l
end
             
type 'a t =
  { mutable arr : 'a list array
  ; mutable max : int }

let capacity t = Array.length t.arr

let set_capacity t cap =
  let addition = Array.create ~len:(cap - (capacity t)) [] in
  let updated = Array.concat [t.arr; addition ] in
  Ok (t.arr <- updated)


let remove t l =
  try
    let _ = Array.set t.arr (Label.to_int l) [] in
    Ok ()
  with
    Invalid_argument msg ->
    Error (Error.of_string msg)
    
let get t l =
  try
    Array.get t.arr (Label.to_int l)
  with
    Invalid_argument _ -> []

let add t l a =
  try
    let idx = Label.to_int l in
    let curr = Array.get t.arr idx in
    let update = a :: curr in
    Ok (Array.set t.arr idx update)
  with
    Invalid_argument msg ->
    Error (Error.of_string msg)
