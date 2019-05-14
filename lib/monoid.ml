include Monoid_intf

module Make (X : S) : S with type 'a t := 'a X.t = struct
  let combine a b = X.combine a b
  let empty = X.empty
end
                                                   
