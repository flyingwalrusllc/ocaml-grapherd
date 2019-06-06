[@@@ocaml.warning "-33"]
open Base
open Graph
[@@@end]

module type S = sig
  type 'a t
     
  val a_star : Label.t Graph.t -> Label.t -> Label.t -> 'a t
end

module type Api = sig
  module type S = S

  include S

end
