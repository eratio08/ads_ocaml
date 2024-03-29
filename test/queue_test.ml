open Ads_ocaml.Queue

let pp fmt (f, r) =
  Format.fprintf fmt "([%a], [%a])" Fmt.(list ~sep:semi int) f Fmt.(list ~sep:semi int) r
;;

let dequeue = Alcotest.testable pp ( = )

let test_cons () =
  let open FiFoDequeue in
  Alcotest.(check dequeue) "cons empty" ([ 1 ], []) (cons 1 empty);
  Alcotest.(check dequeue) "cons non-empty" ([ 2 ], [ 1 ]) (empty |> cons 1 |> cons 2);
  Alcotest.(check dequeue)
    "cons longer non-empty"
    ([ 3; 2 ], [ 1 ])
    (empty |> cons 1 |> cons 2 |> cons 3)
;;

let test_snoc () =
  let open FiFoDequeue in
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
  let open FiFoDequeue in
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
  let open FiFoDequeue in
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

open Ads_ocaml.Stream

module TestStream = struct
  let rec pp fmt = function
    | Stream.Nil -> Format.fprintf fmt "Nil"
    | Stream.Cons (lazy (x, s)) ->
      Format.fprintf fmt "Cons (lazy (%d, %a))" x (fun fmt -> pp fmt) s
  ;;

  let rec equal t1 t2 =
    let open Stream in
    match t1, t2 with
    | Nil, Nil -> true
    | Cons (lazy (h1, t1)), Cons (lazy (h2, t2)) when h1 = h2 -> equal t1 t2
    | _ -> false
  ;;
end

module TestStreamQueue = struct
  let pp fmt = function
    | _, Stream.Nil, _, _ -> Format.fprintf fmt "(0, Nil, 0, Nil)"
    | len_f, f, len_r, r ->
      Format.fprintf fmt "(%d, %a, %d, %a)" len_f TestStream.pp f len_r TestStream.pp r
  ;;

  let equal t1 t2 =
    let open Stream in
    match t1, t2 with
    | (_, Nil, _, _), (_, Nil, _, _) -> true
    | (len_f1, f1, len_r1, r1), (len_f2, f2, len_r2, r2) ->
      len_f1 = len_f2
      && len_r1 = len_r2
      && TestStream.equal f1 f2
      && TestStream.equal r1 r2
  ;;
end

let stream_queue = Alcotest.testable TestStreamQueue.pp TestStreamQueue.equal

let test_snoc_stream () =
  let open Stream in
  let open StreamQueue in
  Alcotest.(check stream_queue)
    "snoc empty"
    (1, Cons (lazy (1, Nil)), 0, Nil)
    (snoc 1 empty);
  Alcotest.(check stream_queue)
    "snoc non-empty"
    (1, Cons (lazy (1, Nil)), 1, Cons (lazy (2, Nil)))
    (snoc 1 empty |> snoc 2);
  Alcotest.(check stream_queue)
    "snoc with re-ordering"
    (3, Cons (lazy (1, Cons (lazy (2, Cons (lazy (3, Nil)))))), 0, Nil)
    (snoc 1 empty |> snoc 2 |> snoc 3)
;;

let test_head_stream () =
  let open Stream in
  let open StreamQueue in
  Alcotest.(check (option int)) "head empty" None (head empty);
  Alcotest.(check (option int))
    "head non-empty"
    (Some 1)
    (head (1, Cons (lazy (1, Nil)), 0, Nil))
;;

let test_tail_stream () =
  let open Stream in
  let open StreamQueue in
  Alcotest.(check (option stream_queue)) "tail empty" None (tail empty);
  Alcotest.(check (option stream_queue))
    "tail non-empty"
    (Some (2, Cons (lazy (2, Cons (lazy (3, Nil)))), 0, Nil))
    (tail (3, Cons (lazy (1, Cons (lazy (2, Cons (lazy (3, Nil)))))), 0, Nil));
  Alcotest.(check (option stream_queue))
    "tail with re-ordering"
    (Some (1, Cons (lazy (2, Nil)), 0, Nil))
    (tail (1, Cons (lazy (1, Nil)), 1, Cons (lazy (2, Nil))))
;;

module TestRealTimeQueue = struct
  include RealTimeQueue

  let pp fmt ((f, r, s) : int t) =
    Format.fprintf
      fmt
      "(%a, %a, %a)"
      TestStream.pp
      f
      Fmt.(brackets (list ~sep:semi int))
      r
      TestStream.pp
      s
  ;;

  let equal (t1 : int t) (t2 : int t) : bool =
    match t1, t2 with
    | (Stream.Nil, _, _), (Stream.Nil, _, _) -> true
    | (f1, r1, s1), (f2, r2, s2) ->
      TestStream.equal f1 f2 && List.equal ( = ) r1 r2 && TestStream.equal s1 s2
  ;;
end

let realtime_queue = Alcotest.testable TestRealTimeQueue.pp TestRealTimeQueue.equal

let test_snoc_realtime () =
  let open RealTimeQueue in
  Alcotest.check
    realtime_queue
    "snoc empty"
    (Cons (lazy (1, Nil)), [], Cons (lazy (1, Nil)))
    (snoc 1 empty);
  Alcotest.check
    realtime_queue
    "snoc non-empty"
    (Cons (lazy (1, Nil)), [ 2 ], Nil)
    (snoc 1 empty |> snoc 2);
  Alcotest.check
    realtime_queue
    "snoc uneven non-empty"
    ( Cons (lazy (3, Cons (lazy (2, Cons (lazy (1, Nil))))))
    , []
    , Cons (lazy (3, Cons (lazy (2, Cons (lazy (1, Nil)))))) )
    (snoc 3 empty |> snoc 2 |> snoc 1);
  Alcotest.check
    realtime_queue
    "snoc even non-empty"
    ( Cons (lazy (3, Cons (lazy (2, Cons (lazy (1, Nil))))))
    , [ 4 ]
    , Cons (lazy (2, Cons (lazy (1, Nil)))) )
    (snoc 3 empty |> snoc 2 |> snoc 1 |> snoc 4)
;;

let test_head_realtime () =
  let open RealTimeQueue in
  Alcotest.(check (option int)) "head empty" None (head empty);
  Alcotest.(check (option int))
    "head non-empty"
    (Some 1)
    (head (Cons (lazy (1, Nil)), [], Cons (lazy (1, Nil))))
;;

let test_tail_realtime () =
  let open RealTimeQueue in
  Alcotest.(check (option realtime_queue)) "tail empty" None (tail empty);
  Alcotest.(check (option realtime_queue))
    "tail non-empty"
    (Some (Nil, [], Nil))
    (tail (Cons (lazy (1, Nil)), [], Cons (lazy (1, Nil))));
  Alcotest.(check (option realtime_queue))
    "tail non-empty"
    (Some (Cons (lazy (2, Cons (lazy (1, Nil)))), [ 4 ], Cons (lazy (1, Nil))))
    (tail
       ( Cons (lazy (3, Cons (lazy (2, Cons (lazy (1, Nil))))))
       , [ 4 ]
       , Cons (lazy (2, Cons (lazy (1, Nil)))) ))
;;

let suite =
  [ "Dequeue.cons", `Quick, test_cons
  ; "Dequeue.snoc", `Quick, test_snoc
  ; "Dequeue.init", `Quick, test_init
  ; "Dequeue.tail", `Quick, test_tail
  ; "StreamQueue.snoc", `Quick, test_snoc_stream
  ; "StreamQueue.head", `Quick, test_head_stream
  ; "StreamQueue.tail", `Quick, test_tail_stream
  ; "RealTimeQueue.snoc", `Quick, test_snoc_realtime
  ; "RealTimeQueue.head", `Quick, test_head_realtime
  ; "RealTimeQueue.tail", `Quick, test_tail_realtime
  ]
;;
