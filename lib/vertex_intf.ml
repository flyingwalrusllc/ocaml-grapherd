module type S = sig
  type label = int [@@deriving compare, equal, sexp]

  type t [@@deriving sexp]

  module Edge : sig
    module T : sig
      type t [@@deriving compare, equal, sexp]

      val id : t -> label

      val weight : t -> float

      val properties : t -> (string * string) list
    end

    val empty : T.t
  end

  val id : t -> label

  val edges : t -> Edge.T.t list
end

module type Vertex = sig
  module type S = S

  include S

  val create : ?weight:float -> ?edges:Edge.T.t list -> label -> t
end
