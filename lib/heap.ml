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
  type e = E.t

  type t =
    | Empty
    | Cons of int * e * t * t

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
    (* the largest heap goes left*)
    | true -> Cons (rank_r + 1, x, l, r)
    | false -> Cons (rank_l + 1, x, r, l)
  ;;

  let rec merge t1 t2 =
    match t1, t2 with
    | t1, Empty -> t1
    | Empty, t2 -> t2
    (* merges are always on the right, as the left is always saturated *)
    | Cons (_, x, l1, r1), (Cons (_, y, _, _) as t2) when x <= y ->
      make_cons x l1 (merge r1 t2)
    | t1, Cons (_, y, l2, r2) -> make_cons y l2 (merge t1 r2)
  ;;

  let insert x t = merge (Cons (1, x, Empty, Empty)) t

  let find_min = function
    | Empty -> failwith "empty"
    | Cons (_, x, _, _) -> x
  ;;

  let delete_min = function
    | Empty -> Empty
    | Cons (_, _, l, r) -> merge l r
  ;;
end

(*
   A binomial tree of rank 0 is a singleton node.
   A binomial tree of rank r + 1 is formed by linking two binomial trees of rank r, making one tree the leftmost child of the other.
   A binomial tree of rank r is a node with r children t1...tr, where each ti is a binomial tree of rank r — i

   Trees of equal rank are always linked using `link`.
*)
module BinomialHeap (E : Ord.Ord) = struct
  type e = E.t
  type n = Node of int * e * n list
  type t = n list

  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false
  ;;

  let root (Node (_, r, _)) = r

  (* Joins two nodes into a new node *)
  let link n1 n2 =
    match n1, n2 with
    | Node (r, x1, cs1), (Node (_, x2, _) as n2) when x1 <= x2 ->
      Node (r + 1, x1, n2 :: cs1)
    | n1, Node (r, x2, cs2) -> Node (r + 1, x2, n1 :: cs2)
  ;;

  let rank (Node (r, _, _)) = r

  (* Insert a node into a tree *)
  let rec ins_tree n t =
    match n, t with
    | t, [] -> [ t ]
    | t, (t' :: _ as ts) when rank t < rank t' -> t :: ts
    | t, t' :: ts' -> ins_tree (link t t') ts'
  ;;

  let insert x t = ins_tree (Node (0, x, [])) t

  let rec merge t1 t2 =
    Fmt.(pr "\nmerge ");
    match t1, t2 with
    | t1, [] -> t1
    | [], t2 -> t2
    | n1 :: r1, (n2 :: _ as t2) when rank n1 < rank n2 -> n1 :: merge r1 t2
    | (n1 :: _ as t1), n2 :: r2 when rank n1 > rank n2 -> n2 :: merge t1 r2
    (* ranks are equal, so join *)
    | n1 :: r1, n2 :: r2 -> ins_tree (link n1 n2) (merge r1 r2)
  ;;

  let rec remove_min_tree = function
    | [] -> failwith "empty"
    | [ t ] -> t, []
    | n :: ns ->
      let n', ns' = remove_min_tree ns in
      (match root n <= root n' with
       | true -> n, ns
       | false -> n', n :: ns')
  ;;

  let find_min ts =
    let t, _ = remove_min_tree ts in
    root t
  ;;

  let delete_min t =
    let Node (_, _, ns1), ns2 = remove_min_tree t in
    merge ns1 ns2
  ;;
end
