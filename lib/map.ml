module type FiniteMap = sig
  type k
  type 'a t

  val empty : 'a t
  val bind : k -> 'a -> 'a t -> 'a t
  val lookup : k -> 'a t -> 'a option
  val lookup_exn : k -> 'a t -> 'a
end

module UnbalancedTreeMap (Key : Ord.Ord) : FiniteMap = struct
  module Elem (Key : Ord.Ord) = struct
    type 'a t = Key.t * 'a

    let compare (k1, _) (k2, _) = compare k1 k2
    let ( < ) (k1, _) (k2, _) = k1 < k2
    let ( <= ) (k1, _) (k2, _) = k1 <= k2
    let ( > ) (k1, _) (k2, _) = k1 > k2
    let ( >= ) (k1, _) (k2, _) = k1 >= k2
    let min (k1, _) (k2, _) = if k1 < k2 then k1 else k2
    let max (k1, _) (k2, _) = if k1 > k2 then k1 else k2
  end

  module MapElem = Elem (Key)

  type 'a tree =
    | Empty
    | Cons of 'a tree * 'a MapElem.t * 'a tree

  type k = Key.t
  type 'a t = 'a tree

  let empty = Empty

  let rec bind k a = function
    | Empty -> Cons (Empty, (k, a), Empty)
    | Cons (l, v, r) when (k, a) = v -> Cons (l, v, r)
    | Cons (l, v, r) when (k, a) < v -> Cons (bind k a l, v, r)
    | Cons (l, v, r) -> Cons (l, v, bind k a r)
  ;;

  let rec lookup k = function
    | Empty -> None
    | Cons (_, (v, a), _) when k = v -> Some a
    | Cons (l, (v, _), _) when k < v -> lookup k l
    | Cons (_, _, r) -> lookup k r
  ;;

  let rec lookup_exn k = function
    | Empty -> failwith "empty"
    | Cons (_, (v, a), _) when k = v -> a
    | Cons (l, (v, _), _) when k < v -> lookup_exn k l
    | Cons (_, _, r) -> lookup_exn k r
  ;;
end
