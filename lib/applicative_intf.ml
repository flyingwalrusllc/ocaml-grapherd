
module type S = sig
  type 'a t

  include Functor.S with type 'a t := 'a t
        
  val pure : 'a -> 'a t
  val ap : 'a t -> ('a -> 'b) t -> 'b t
end
              
module type Applicative = sig
  module type S = S

  module Make(X : S) : S with type 'a t := 'a X.t

end
                       
                        
                               
