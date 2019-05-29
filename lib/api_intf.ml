open Base
open Graph

module type S = sig
  
  val traverse_breadth_first : Label.t Graph.t -> ?depth:int -> Label.t -> Label.t list array

  val traverse_depth_first : Label.t Graph.t -> f:(Label.t -> 'b) -> Label.t -> 'b list
    
end

module type Api = sig
  module type S = S

  include S
end
                    
