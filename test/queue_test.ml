open Ads_ocaml.Queue

let pp fmt (f, r) =
  Format.fprintf
    fmt
    "([%a], [%a])"
    Fmt.(list ~sep:(any "; ") int)
    f
    Fmt.(list ~sep:(any "; ") int)
    r
;;

let dequeue = Alcotest.testable pp ( = )

let test_cons () =
  let open FiFo_dequeue in
  Alcotest.(check dequeue) "cons empty" ([ 1 ], []) (cons 1 empty);
  Alcotest.(check dequeue) "cons non-empty" ([ 2 ], [ 1 ]) (empty |> cons 1 |> cons 2);
  Alcotest.(check dequeue)
    "cons longer non-empty"
    ([ 3; 2 ], [ 1 ])
    (empty |> cons 1 |> cons 2 |> cons 3)
;;

let test_snoc () =
  let open FiFo_dequeue in
  Alcotest.(check dequeue) "snoc empty" ([ 1 ], []) (snoc 1 empty);
  Alcotest.(check dequeue)
    "snoc non-empty"
    ([ 1 ], [ 5; 4; 3; 2 ])
    (empty |> snoc 1 |> snoc 2 |> snoc 3 |> snoc 4 |> snoc 5);
  Alcotest.(check dequeue)
    "snoc cons non-empty"
    ([ 4; 2 ], [ 5; 3; 1 ])
    (empty |> snoc 1 |> cons 2 |> snoc 3 |> cons 4 |> snoc 5)
;;

let test_init () =
  let open FiFo_dequeue in
  Alcotest.(check (option dequeue)) "init empty" None (init empty);
  Alcotest.(check (option dequeue)) "init non-empty" (Some ([ 1 ], [])) (init ([ 1 ], []));
  Alcotest.(check (option dequeue))
    "init longer non-empty"
    (Some ([ 1 ], [ 3; 2 ]))
    (init ([ 1; 2; 3 ], [ 4 ]));
  Alcotest.(check (option dequeue))
    "init longer non-empty"
    (Some ([ 1 ], [ 3; 2 ]))
    (init ([ 1 ], [ 4; 3; 2 ]))
;;

let test_tail () =
  let open FiFo_dequeue in
  Alcotest.(check (option dequeue)) "init empty" None (tail empty);
  Alcotest.(check (option dequeue)) "init non-empty" (Some ([], [])) (tail ([ 1 ], []));
  Alcotest.(check (option dequeue))
    "tail longer non-empty"
    (Some ([ 2; 3 ], [ 4 ]))
    (tail ([ 1; 2; 3 ], [ 4 ]));
  Alcotest.(check (option dequeue))
    "tail longer non-empty"
    (Some ([ 2; 3 ], [ 4 ]))
    (tail ([ 1 ], [ 4; 3; 2 ]))
;;

let suite =
  [ "Dequeue.cons", `Quick, test_cons
  ; "Dequeue.snoc", `Quick, test_snoc
  ; "Dequeue.init", `Quick, test_init
  ; "Dequeue.tail", `Quick, test_tail
  ]
;;
