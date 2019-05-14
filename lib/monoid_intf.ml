module type S = sig
  type 'a t

  val combine : 'a t -> 'a t -> 'a t
  val empty : 'a t
end
              
module type Monoid = sig
  module type S = S

  module Make (X : S) : S with type 'a t := 'a X.t
end
