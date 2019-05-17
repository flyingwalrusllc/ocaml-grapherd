open Base

module type S = sig
  type 'a t

  val map : 'a t -> ('a -> 'b) -> 'b t
end

module type Functor = sig
  module type S = S

  module Make (X : S) : S with type 'a t := 'a X.t

  module List : S with type 'a t := 'a list

  module Option : S with type 'a t := 'a option

  module Result : S with type 'a t := 'a Or_error.t
end
