type 'a t

val of_array : 'a array -> ('a -> int) -> (int -> 'a) -> 'a t

val get : 'a t -> int -> 'a

val set : 'a t -> int -> 'a -> unit

val length : 'a t -> int

val grow : 'a t -> int -> 'a t
