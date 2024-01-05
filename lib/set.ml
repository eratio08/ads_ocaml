module type Set = sig
  type e
  type t

  val empty : t
  val insert : e -> t -> t
  val member : e -> t -> bool
end

module UnbalancedTreeSet (Elem : Ord.Ord) : Set = struct
  module T = Tree.Tree (Elem)
  include T

  type e = Elem.t

  let empty = Empty

  let rec insert e = function
    | Empty -> Cons (Empty, e, Empty)
    | Cons (_, v, _) as t when compare e v = 0 -> t
    | Cons (l, v, r) when e < v -> Cons (insert e l, v, r)
    | Cons (l, v, r) -> Cons (l, v, insert e r)
  ;;

  let rec member e = function
    | Empty -> false
    | Cons (_, v, _) when compare e v = 0 -> true
    | Cons (l, v, _) when e < v -> member e l
    | Cons (_, _, r) -> member e r
  ;;
end
