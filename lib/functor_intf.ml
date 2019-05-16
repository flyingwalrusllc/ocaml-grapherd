
module type S = sig
  type 'a t

  val map : 'a t -> ('a -> 'b) -> 'b t
end

module type Functor = sig
  module type S = S

  module Make(X : S) : S with type 'a t := 'a X.t

end
                       
                        
                               
