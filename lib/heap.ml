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

module ExplicitMin (H : Heap) = struct
  type e = H.e

  type t =
    | E
    | NE of e * H.t

  let empty = E

  let is_empty = function
    | E -> true
    | _ -> false
  ;;

  let insert x = function
    | E -> NE (x, H.empty)
    | NE (min, h) when x < min -> NE (x, H.insert x h)
    | NE (_, h) -> NE (x, H.insert x h)
  ;;

  let merge t1 t2 =
    match t1, t2 with
    | E, E -> E
    | t, E | E, t -> t
    | NE (x, t1), NE (y, t2) when x <= y -> NE (x, H.merge t1 t2)
    | NE (_, t1), NE (y, t2) -> NE (y, H.merge t1 t2)
  ;;

  let find_min = function
    | E -> failwith "empty"
    | NE (min, _) -> min
  ;;

  let delete_min = function
    | E -> failwith "empty"
    | NE (_, t) ->
      let t = H.delete_min t in
      let x = H.find_min t in
      NE (x, t)
  ;;
end

module SplayHeap (Elem : Ord.Ord) = struct
  type e = Elem.t

  type t =
    | Empty
    | Tree of t * e * t

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false
  ;;

  (* filter out all elements smaller or equal than pivot, keep only bigger *)
  let rec bigger pivot = function
    | Empty -> Empty
    | Tree (_, x, b) when x <= pivot -> bigger pivot b
    | Tree (a, x, b) ->
      (match a with
       | Empty -> Tree (Empty, x, b)
       | Tree (a1, y, _) when y <= pivot -> Tree (bigger pivot a1, x, b)
       (* rotate if gone left twice *)
       | Tree (a1, y, a2) -> Tree (bigger pivot a1, y, Tree (a2, x, b)))
  ;;

  (* filter out all bigger elements, keep only smaller or equal *)
  let rec smaller pivot = function
    | Empty -> Empty
    | Tree (a, x, _) when x > pivot -> smaller pivot a
    | Tree (a, x, b) ->
      (match b with
       | Empty -> Tree (a, x, Empty)
       | Tree (_, y, b2) when y > pivot -> Tree (a, x, smaller x b2)
       | Tree (b1, y, b2) -> Tree (Tree (b1, x, a), y, smaller pivot b2))
  ;;

  (* return a pair of trees so that fst has only smaller or equal and snd bigger elements *)
  let rec partition pivot = function
    | Empty -> Empty, Empty
    | Tree (a, x, b) as t when x <= pivot ->
      (match b with
       | Empty -> t, Empty
       (* x <= y <= pivot *)
       | Tree (b1, y, b2) when y <= pivot ->
         let small, big = partition pivot b2 in
         Tree (Tree (a, x, b1), y, small), big
       (* x <= pivot < y *)
       | Tree (b1, y, b2) ->
         let small, big = partition pivot b1 in
         Tree (a, x, small), Tree (big, y, b2))
    (* pivot < x *)
    | Tree (a, x, b) as t ->
      (match a with
       | Empty -> Empty, t
       (* y <= pivot < x *)
       | Tree (a1, y, a2) when y <= pivot ->
         let small, big = partition pivot a2 in
         Tree (a1, y, small), Tree (big, x, b)
       (* pivot < y < x *)
       | Tree (a1, y, a2) ->
         let small, big = partition pivot a1 in
         small, Tree (big, y, Tree (a2, x, b)))
  ;;

  let insert' x t = Tree (smaller x t, x, bigger x t)

  let insert x t =
    let small, big = partition x t in
    Tree (small, x, big)
  ;;

  let rec find_min = function
    | Empty -> failwith "empty"
    | Tree (Empty, x, _) -> x
    | Tree (a, _, _) -> find_min a
  ;;

  let rec delete_min = function
    | Empty -> failwith "empty"
    | Tree (Empty, _, b) -> b
    | Tree (Tree (Empty, _, b), y, c) -> Tree (b, y, c)
    | Tree (Tree (a, x, b), y, c) -> Tree (delete_min a, x, Tree (b, y, c))
  ;;

  let rec merge t1 t2 =
    match t1, t2 with
    | Empty, t -> t
    | Tree (a, x, b), t ->
      let small, big = partition x t in
      Tree (merge small a, x, merge big b)
  ;;
end

module PairingHeap (Elem : Ord.Ord) = struct
  type e = Elem.t

  type t =
    | E
    | T of e * t list

  let empty = E

  let is_empty = function
    | E -> true
    | _ -> false
  ;;

  let find_min = function
    | E -> failwith "empty"
    | T (x, _) -> x
  ;;

  (* merge makes the tree with the larger root the leftmost child of the tree with the smaller root *)
  let merge t1 t2 =
    match t1, t2 with
    | E, h | h, E -> h
    | T (x, hs1), (T (y, _) as h2) when x <= y -> T (x, h2 :: hs1)
    (* x > y *)
    | h1, T (y, hs2) -> T (y, h1 :: hs2)
  ;;

  let insert x t = merge (T (x, [])) t

  (* merges children in pairs from left to right then merges the resulting trees from right to left *)
  let rec merge_pairs = function
    | [] -> E
    | [ h ] -> h
    | h1 :: h2 :: hs -> merge (merge h1 h2) (merge_pairs hs)
  ;;

  (* discards the root and then merges the children *)
  let delete_min = function
    | E -> failwith "empty"
    | T (_, hs) -> merge_pairs hs
  ;;
end
