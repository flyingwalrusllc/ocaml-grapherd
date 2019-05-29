open Base

module type S = sig
  type 'a t

  module Label : sig
    module T : sig
      type t

      val of_int : int -> t

      val to_int : t -> int

      val equal : t -> t -> bool

      val compare : t -> t -> int

      val sexp_of_t : t -> Sexp.t
    end

    type t = T.t

    type comparator_witness = Comparator.Make(T).comparator_witness

    val comparator : (t, comparator_witness) Comparator.t
  end

  val get : 'a t -> Label.t -> 'a list
  (** [get graph label] get all edges for vertex label *)

  val add : 'a t -> Label.t -> 'a -> 'a t Or_error.t
  (** [add graph label a] add edge a to vertex label *)

  val clear : 'a t -> Label.t -> 'a t Or_error.t

  val remove : 'a t -> Label.t -> 'a -> 'a t Or_error.t
end

module type Graph = sig
  module type S = S

  include S

  val capacity : 'a t -> int

  val set_capacity : 'a t -> int -> 'a t Or_error.t

  val create : int -> 'a t
end
