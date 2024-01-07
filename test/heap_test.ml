open Ads_ocaml.Heap

module IntLeftistHeap = struct
  module OrdInt = struct
    include Ads_ocaml.Ord.Ord (Int)

    let pp fmt t = Format.fprintf fmt "%d" t
  end

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

let suite =
  [ "LeftistHeap.merge", `Quick, test_merge
  ; "LeftistHeap.insert", `Quick, test_insert
  ; "LeftistHeap.delete_min", `Quick, test_delete_min
  ; "BinomialHeap.merge", `Quick, test_merge_binomial
  ; "BinomialHeap.insert", `Quick, test_insert_binomial
  ; "BinomialHeap.find_min", `Quick, test_find_min_binomial
  ]
;;
