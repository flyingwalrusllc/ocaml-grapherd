include Semigroup_intf

module Make (X : S) : S with type 'a t := 'a X.t = struct
  let combine a b = X.combine a b
end

module List_semigroup = Make (struct
  type 'a t = 'a list

  let combine a b = List.append a b
end)
