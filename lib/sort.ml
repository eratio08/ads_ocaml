module type Sortable = functor (Elem : Ord.Ord) -> sig
  type t
  type e = Elem.t

  val empty : t
  val add : e -> t -> t
  val sort : t -> e list
end
