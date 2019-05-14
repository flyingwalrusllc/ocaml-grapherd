open Base

module type S = sig
  type 'a t

  module Label : sig
    type t

    val of_int : int -> t
    val to_int : t -> int
  end

  val get : 'a t -> Label.t -> 'a list

  val add : 'a t -> Label.t -> 'a -> unit Or_error.t

  val remove : 'a t -> Label.t -> unit Or_error.t
end

module type Graph = sig

  module type S = S

  include S

  val capacity : 'a t -> int

  val set_capacity : 'a t -> int -> unit Or_error.t
end
                      
                    
