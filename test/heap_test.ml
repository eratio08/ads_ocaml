open Ads_ocaml.Heap

module IntLeftistHeap = struct
  include LeftistHeap (Ads_ocaml.Ord.Ord (Int))

  let rec pp p = function
    | Empty -> Format.fprintf p "Empty"
    | Cons (rank, v, l, r) ->
      Format.fprintf p "Cons (%d,%d,%a,%a)" rank v (fun p -> pp p) l (fun p -> pp p) r
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

let suite =
  [ "LeftistHeap.merge", `Quick, test_merge
  ; "LeftistHeap.insert", `Quick, test_insert
  ; "LeftistHeap.delete_min", `Quick, test_delete_min
  ]
;;
