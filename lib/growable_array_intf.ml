module type Growable_array = sig
  type 'a t

  val of_array : 'a array -> 'a -> 'a t

  val get : 'a t -> int -> 'a

  val set : 'a t -> int -> 'a -> unit

  val max : 'a t -> int

  val grow : 'a t -> int -> 'a t
end
