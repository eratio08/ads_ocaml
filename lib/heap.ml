module type Heap = sig
  type e
  type t

  val empty : t
  val is_empty : t -> bool
  val insert : e -> t -> t
  val merge : t -> t -> t
  val find_min : t -> e
  val delete_min : t -> t
end

module LeftistHeap (E : Ord.Ord) = struct
  type t =
    | Empty
    | Cons of int * E.t * t * t

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false
  ;;

  let rank = function
    | Empty -> 0
    | Cons (r, _, _, _) -> r
  ;;

  let make_cons x l r =
    let rank_l = rank l in
    let rank_r = rank r in
    match rank_l >= rank_r with
    | true -> Cons (rank_r + 1, x, l, r)
    | false -> Cons (rank_l + 1, x, r, l)
  ;;

  let rec merge t1 t2 =
    match t1, t2 with
    | t1, Empty -> t1
    | Empty, t2 -> t2
    | Cons (_, x, l1, r1), (Cons (_, y, _, _) as t2) when x <= y ->
      make_cons x l1 (merge r1 t2)
    | t1, Cons (_, y, l2, r2) -> make_cons y l2 (merge t1 r2)
  ;;

  let insert x t = merge (Cons (1, x, Empty, Empty)) t

  let find_min = function
    | Empty -> failwith "empty"
    | Cons (_, x, _, _) -> x
  ;;
end
