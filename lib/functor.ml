include Functor_intf

module Make (X : S) : S with type 'a t := 'a X.t = struct
  let map a func = X.map a func
end
                                                   
