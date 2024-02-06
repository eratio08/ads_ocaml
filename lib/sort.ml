module type Sortable = sig
  type t
  type e

  val empty : t
  val add : e -> t -> t
  val sort : t -> e list
end

(* module SortableList (Elem : Ord.Ord) : Sortable with type e = Elem.t and type t = int list = struct *)
(**)
(* end *)
