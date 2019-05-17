include Applicative_intf

module Make (X : S) : S with type 'a t := 'a X.t = struct
  let pure a = X.pure a

  let ap a func = X.ap a func

  let map a func = ap a (pure func)
end
