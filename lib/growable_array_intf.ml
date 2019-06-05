include Base
      
module type Growable_array = sig
  type 'a t
        
  val create : int -> 'a -> 'a t
     
  val of_array : 'a array -> 'a -> 'a t
  
  val get : 'a t -> int -> 'a
  
  val set : 'a t -> int -> 'a -> unit
  
  val max : 'a t -> int
  
  val capacity : 'a t -> int

  val ensure : 'a t -> int -> unit
    
end
