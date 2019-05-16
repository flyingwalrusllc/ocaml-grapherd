include Monoid_intf
open Base

module Make (X : S) : S with type 'a t := 'a X.t = struct
  let combine a b = X.combine a b

  let empty = X.empty
end

module List = Make (struct
  type 'a t = 'a list

  let empty = []

  let combine a b = List.append a b
end)
