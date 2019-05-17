module type S = sig
  type 'a t

  include Semigroup.S with type 'a t := 'a t

  val empty : 'a t
end

module type Monoid = sig
  module type S = S

  module Make (X : S) : S with type 'a t := 'a X.t

  module List : S with type 'a t := 'a list

end
