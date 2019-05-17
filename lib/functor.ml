include Functor_intf
open Base

module Make (X : S) : S with type 'a t := 'a X.t = struct
  let map a func = X.map a func
end

module List = Make (struct
  type 'a t = 'a list

  let map a f = List.map a ~f
end)

module Option = Make (struct
  type 'a t = 'a option

  let map o f = Option.map o ~f
end)

module Result = Make (struct
  type 'a t = 'a Or_error.t

  let map r f = Or_error.map r ~f
end)
