open Ads_ocaml.Heap

module OrdInt = struct
  include Ads_ocaml.Ord.Ord (Int)

  let pp fmt t = Format.fprintf fmt "%d" t
end

module IntLeftistHeap = struct
  include LeftistHeap (OrdInt)

  let rec pp p = function
    | Empty -> Format.fprintf p "Empty"
    | Cons (rank, v, l, r) ->
      Format.fprintf
        p
        "Cons (%d,%a,%a,%a)"
        rank
        OrdInt.pp
        v
        (fun p -> pp p)
        l
        (fun p -> pp p)
        r
  ;;
end

let leftist_heap = Alcotest.testable IntLeftistHeap.pp ( = )

let test_merge () =
  let open IntLeftistHeap in
  Alcotest.(check leftist_heap) "merge empty" empty (merge empty empty);
  Alcotest.(check leftist_heap)
    "merge empty and non empty"
    (Cons (1, 1, empty, empty))
    (merge empty (Cons (1, 1, empty, empty)));
  Alcotest.(check leftist_heap)
    "merge two non-empty"
    (Cons (2, 1, Cons (1, 2, Empty, Empty), Cons (1, 5, Cons (1, 9, Empty, Empty), Empty)))
    (merge
       (Cons (1, 9, empty, empty))
       (merge
          (Cons (1, 5, empty, empty))
          (merge (Cons (1, 2, empty, empty)) (Cons (1, 1, empty, empty)))));
  Alcotest.(check leftist_heap)
    "merge non-empty revers"
    (Cons (1, 1, Cons (1, 2, Cons (1, 5, Cons (1, 9, Empty, Empty), Empty), Empty), Empty))
    (merge
       (Cons (1, 1, empty, empty))
       (merge
          (Cons (1, 2, empty, empty))
          (merge (Cons (1, 5, empty, empty)) (Cons (1, 9, empty, empty)))))
;;

let test_insert () =
  let open IntLeftistHeap in
  Alcotest.(check leftist_heap)
    "insert into empty"
    (Cons (1, 1, empty, empty))
    (insert 1 empty);
  Alcotest.(check leftist_heap)
    "insert into non-empty bigger value"
    (Cons (1, 1, Cons (1, 9, empty, empty), empty))
    (insert 9 (Cons (1, 1, empty, empty)));
  Alcotest.(check leftist_heap)
    "insert into non-empty smaller value"
    (Cons (1, 1, Cons (1, 9, empty, empty), empty))
    (insert 1 (Cons (1, 9, empty, empty)))
;;

let test_delete_min () =
  let open IntLeftistHeap in
  Alcotest.(check leftist_heap) "delete from empty" empty (delete_min empty);
  Alcotest.(check leftist_heap)
    "delete from shallow"
    empty
    (delete_min @@ Cons (1, 1, empty, empty));
  Alcotest.(check leftist_heap)
    "delete from deep"
    (Cons (1, 3, Cons (1, 9, empty, empty), empty))
    (delete_min @@ Cons (1, 1, Cons (1, 3, empty, empty), Cons (1, 9, empty, empty)))
;;

module CharBinomialHeap = struct
  module OrdChar = struct
    include Ads_ocaml.Ord.Ord (Char)

    let pp p c = Format.fprintf p "'%c'" c
  end

  include BinomialHeap (OrdChar)

  let rec pp fmt = function
    | [] -> Format.fprintf fmt "[]"
    | Node (r, c, ns) :: ts ->
      Format.fprintf
        fmt
        "Node (%d, %a, %a) :: %a"
        r
        OrdChar.pp
        c
        (fun fmt -> pp fmt)
        ns
        (fun fmt -> pp fmt)
        ts
  ;;
end

let binomial_heap = Alcotest.testable CharBinomialHeap.pp ( = )

let test_merge_binomial () =
  let open CharBinomialHeap in
  Alcotest.(check binomial_heap) "merge empty" empty (merge empty empty);
  Alcotest.(check binomial_heap)
    "merge empty & non-empty"
    [ Node (0, 'a', []) ]
    (merge empty [ Node (0, 'a', []) ]);
  Alcotest.(check binomial_heap)
    "merge shallow"
    (Node (1, 'a', Node (0, 'b', []) :: []) :: [])
    (merge [ Node (0, 'a', []) ] [ Node (0, 'b', []) ]);
  Alcotest.(check binomial_heap)
    "merge deep"
    [ Node (1, 'a', Node (0, 'z', []) :: [])
    ; Node (2, 'b', [ Node (1, 'c', Node (0, 'd', []) :: []); Node (0, 'y', []) ])
    ]
    (merge
       (Node (2, 'b', [ Node (1, 'c', Node (0, 'd', []) :: []); Node (0, 'y', []) ]) :: [])
       (Node (1, 'a', Node (0, 'z', []) :: []) :: []))
;;

let test_insert_binomial () =
  let open CharBinomialHeap in
  Alcotest.(check binomial_heap)
    "insert into empty"
    [ Node (0, 'a', []) ]
    (insert 'a' empty);
  Alcotest.(check binomial_heap)
    "insert into non-empty shallow"
    [ Node (1, 'a', [ Node (0, 'c', []) ]) ]
    (insert 'a' [ Node (0, 'c', []) ]);
  Alcotest.(check binomial_heap)
    "insert into non-empty deep"
    [ Node (0, 'b', [])
    ; Node (2, 'a', [ Node (1, 'c', Node (0, 'x', []) :: []); Node (0, 'z', []) ])
    ]
    (insert 'a' empty |> insert 'z' |> insert 'c' |> insert 'x' |> insert 'b')
;;

let test_find_min_binomial () =
  let open CharBinomialHeap in
  Alcotest.(check char) "min of shallow" 'c' (find_min [ Node (0, 'c', []) ]);
  Alcotest.(check char)
    "min of shallow"
    'a'
    (find_min
       [ Node (0, 'b', [])
       ; Node (2, 'a', [ Node (1, 'c', Node (0, 'x', []) :: []); Node (0, 'z', []) ])
       ])
;;

module IntSplayHeap = struct
  include Ads_ocaml.Heap.SplayHeap (OrdInt)

  let rec pp fmt = function
    | Empty -> Format.fprintf fmt "Empty"
    | Tree (a, x, b) ->
      Format.fprintf
        fmt
        "Tree (%a, %a, %a)"
        (fun fmt -> pp fmt)
        a
        OrdInt.pp
        x
        (fun fmt -> pp fmt)
        b
  ;;
end

let splayheap = Alcotest.testable IntSplayHeap.pp ( = )

let test_insert_splay () =
  let open IntSplayHeap in
  Alcotest.(check splayheap) "insert empty" (Tree (empty, 1, empty)) (insert 1 empty);
  Alcotest.(check splayheap)
    "insert non-empty"
    (Tree (Tree (Tree (Tree (Empty, 1, Empty), 2, Empty), 3, Empty), 4, Empty))
    (insert 1 empty |> insert 2 |> insert 3 |> insert 4);
  Alcotest.(check splayheap)
    "insert non-empty"
    (Tree
       ( Tree (Empty, 1, Empty)
       , 1
       , Tree (Tree (Empty, 2, Empty), 3, Tree (Empty, 4, Empty)) ))
    (insert 1 (Tree (Tree (Tree (Tree (Empty, 1, Empty), 2, Empty), 3, Empty), 4, Empty)));
  Alcotest.(check splayheap) "insert' empty" (Tree (empty, 1, empty)) (insert' 1 empty);
  Alcotest.(check splayheap)
    "insert' non-empty"
    (Tree (Tree (Tree (Tree (Empty, 1, Empty), 2, Empty), 3, Empty), 4, Empty))
    (insert' 1 empty |> insert' 2 |> insert' 3 |> insert' 4);
  Alcotest.(check splayheap)
    "insert' non-empty"
    (Tree
       ( Tree (Empty, 1, Empty)
       , 1
       , Tree (Tree (Empty, 2, Empty), 3, Tree (Empty, 4, Empty)) ))
    (insert'
       1
       (Tree (Tree (Tree (Tree (Empty, 1, Empty), 2, Empty), 3, Empty), 4, Empty)))
;;

let test_find_min_splay () =
  let open IntSplayHeap in
  Alcotest.check_raises "find_min empty" (Failure "empty") (fun () ->
    find_min empty |> ignore);
  Alcotest.(check int) "find_min non-empty" 1 (find_min (insert 1 empty));
  Alcotest.(check int)
    "find_min non-empty"
    1
    (find_min (insert 1 empty |> insert 3 |> insert 2))
;;

let test_delete_min_splay () =
  let open IntSplayHeap in
  Alcotest.check_raises "delete_min empty" (Failure "empty") (fun () ->
    delete_min empty |> ignore);
  Alcotest.(check splayheap)
    "delete min non-empty"
    (Tree (Empty, 2, Empty))
    (delete_min (insert 1 empty |> insert 2))
;;

module ExplicitMinIntSplayHeap = struct
  include ExplicitMin (IntSplayHeap)

  let pp fmt = function
    | E -> Format.fprintf fmt "E"
    | NE (x, t) -> Format.fprintf fmt "NE (%d, %a)" x IntSplayHeap.pp t
  ;;
end

let explicit_splayheap = Alcotest.testable ExplicitMinIntSplayHeap.pp ( = )

let test_find_min_explicit () =
  let open ExplicitMinIntSplayHeap in
  Alcotest.check_raises "find_min empty" (Failure "empty") (fun () ->
    find_min empty |> ignore)
;;

let test_delete_min_explicit () =
  let open ExplicitMinIntSplayHeap in
  Alcotest.check_raises "delete_min empty" (Failure "empty") (fun () ->
    delete_min empty |> ignore);
  Alcotest.(check explicit_splayheap)
    "delete_min non-empty"
    (NE (2, IntSplayHeap.Tree (IntSplayHeap.Empty, 2, IntSplayHeap.Empty)))
    (delete_min
       (NE (1, IntSplayHeap.insert 2 IntSplayHeap.empty |> IntSplayHeap.insert 2)))
;;

module IntPairingHeap = struct
  include PairingHeap (OrdInt)

  let rec pp fmt = function
    | E -> Format.fprintf fmt "E"
    | T (e, ts) ->
      Format.fprintf
        fmt
        "T(%a, [%a])"
        OrdInt.pp
        e
        Fmt.(list ~sep:semi (fun fmt -> pp fmt))
        ts
  ;;
end

let pairing_heap = Alcotest.testable IntPairingHeap.pp ( = )

let test_find_min_pairing () =
  let open IntPairingHeap in
  Alcotest.check_raises "find_min empty" (Failure "empty") (fun () ->
    find_min empty |> ignore);
  Alcotest.(check int) "find_min non-empty" 1 (find_min (T (1, [])))
;;

let test_insert_pairing () =
  let open IntPairingHeap in
  Alcotest.(check pairing_heap) "insert empty" (T (1, [])) (insert 1 empty);
  Alcotest.(check pairing_heap)
    "insert non-empty"
    (T (1, [ T (2, []); T (3, []) ]))
    (insert 3 empty |> insert 1 |> insert 2);
  Alcotest.(check pairing_heap)
    "insert inverse non-empty"
    (T (1, [ T (2, [ T (3, [ T (4, [ T (5, []) ]) ]) ]) ]))
    (insert 5 empty |> insert 4 |> insert 3 |> insert 2 |> insert 1)
;;

let test_delete_min_pairing () =
  let open IntPairingHeap in
  Alcotest.check_raises "delete_min empty" (Failure "empty") (fun () ->
    delete_min empty |> ignore);
  Alcotest.(check pairing_heap) "delete_min non-empty" empty (insert 1 empty |> delete_min);
  Alcotest.(check pairing_heap)
    "delete_min deeper non-empty"
    (T (2, [ T (3, [ T (4, [ T (5, []) ]) ]) ]))
    (insert 5 empty |> insert 4 |> insert 3 |> insert 2 |> insert 1 |> delete_min);
  Alcotest.(check pairing_heap)
    "delete_min non linear"
    (T (2, [ T (3, []) ]))
    (delete_min (insert 3 empty |> insert 1 |> insert 2))
;;

module CharLazyBinomialHeap = struct
  module OrdChar = struct
    include Ads_ocaml.Ord.Ord (Char)

    let pp p c = Format.fprintf p "'%c'" c
  end

  include LazyBinomialHeap (OrdChar)

  let rec pp fmt = function
    | (lazy []) -> Format.fprintf fmt "[]"
    | (lazy (Node (r, c, ns) :: ts)) ->
      Format.fprintf
        fmt
        "Node (%d, %a, %a) :: %a"
        r
        OrdChar.pp
        c
        (fun fmt -> pp fmt)
        (lazy ns)
        (fun fmt -> pp fmt)
        (lazy ts)
  ;;

  let rec equal_node n1 n2 =
    match n1, n2 with
    | Node (r1, x1, []), Node (r2, x2, []) when r1 = r2 && x1 = x2 -> true
    | Node (r1, x1, c1), Node (r2, x2, c2)
      when r1 = r2 && x1 = x2 && List.length c1 = List.length c2 ->
      List.fold_left2 (fun acc n1 n2 -> acc && equal_node n1 n2) true c1 c2
    | _, _ -> false
  ;;

  let rec equal t1 t2 =
    match t1, t2 with
    | (lazy []), (lazy []) -> true
    | (lazy (n1 :: ns1)), (lazy (n2 :: ns2)) when equal_node n1 n2 ->
      equal (lazy ns1) (lazy ns2)
    | _ -> false
  ;;
end

let lazy_binomial_heap =
  Alcotest.testable CharLazyBinomialHeap.pp CharLazyBinomialHeap.equal
;;

let test_is_empty_lazy_binomial () =
  let open CharLazyBinomialHeap in
  Alcotest.(check bool) "is_empty empty" true (is_empty empty);
  Alcotest.(check bool) "is_empty non-empty" false (is_empty (lazy [ Node (1, 'a', []) ]))
;;

let test_insert_lazy_binomial () =
  let open CharLazyBinomialHeap in
  Alcotest.(check lazy_binomial_heap)
    "insert empty"
    (lazy [ Node (0, 'a', []) ])
    (insert 'a' empty);
  Alcotest.(check lazy_binomial_heap)
    "insert non-empty"
    (lazy [ Node (0, 'a', []); Node (1, 'x', Node (0, 'z', []) :: []) ])
    (insert 'z' empty |> insert 'x' |> insert 'a')
;;

let test_find_min_lazy_binomial () =
  let open CharLazyBinomialHeap in
  Alcotest.(check_raises) "find_min empty" (Failure "empty") (fun () ->
    find_min empty |> ignore);
  Alcotest.(check char) "find_min non-empty" 'a' (insert 'a' empty |> find_min);
  Alcotest.(check char)
    "find_min non-empty"
    'a'
    (insert 'x' empty |> insert 'z' |> insert 'a' |> find_min)
;;

let suite =
  [ "LeftistHeap.merge", `Quick, test_merge
  ; "LeftistHeap.insert", `Quick, test_insert
  ; "LeftistHeap.delete_min", `Quick, test_delete_min
  ; "BinomialHeap.merge", `Quick, test_merge_binomial
  ; "BinomialHeap.insert", `Quick, test_insert_binomial
  ; "BinomialHeap.find_min", `Quick, test_find_min_binomial
  ; "SplayHeap.insert", `Quick, test_insert_splay
  ; "SplayHeap.find_min", `Quick, test_find_min_splay
  ; "SplayHeap.delete_min", `Quick, test_delete_min_splay
  ; "ExplicitSplayHeap.find_min", `Quick, test_find_min_explicit
  ; "ExplicitSplayHeap.delete_min", `Quick, test_delete_min_explicit
  ; "PairingHeap.find_min", `Quick, test_find_min_pairing
  ; "PairingHeap.insert", `Quick, test_insert_pairing
  ; "PairingHeap.delete_min", `Quick, test_delete_min_pairing
  ; "LazyBinomialHeap.is_empty", `Quick, test_is_empty_lazy_binomial
  ; "LazyBinomialHeap.insert", `Quick, test_insert_lazy_binomial
  ; "LazyBinomialHeap.find_min", `Quick, test_find_min_lazy_binomial
  ]
;;
