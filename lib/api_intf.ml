open Base
open Graph

module type S = sig

  val breadth_first_search : Label.t Graph.t -> Label.t -> Label.t -> Label.t list option
    
  val traverse_breadth_first : Label.t Graph.t -> ?depth:int -> Label.t -> Label.t list array

  val traverse_depth_first : Label.t Graph.t -> f:(Label.t -> 'b) -> Label.t -> 'b list
    
end

module type Api = sig
  module type S = S

  include S
end
                    
