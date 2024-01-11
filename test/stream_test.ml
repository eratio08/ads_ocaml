open Ads_ocaml.Stream

module Stream_test = struct
  let rec pp fmt = function
    | Stream.Nil -> Format.fprintf fmt "Nil"
    | Stream.Cons (lazy (x, s)) ->
      Format.fprintf fmt "Cons (lazy (\"%s\", %a))" x (fun fmt -> pp fmt) s
  ;;

  let rec equal t1 t2 =
    let open Stream in
    match t1, t2 with
    | Nil, Nil -> true
    | Cons (lazy (h1, t1)), Cons (lazy (h2, t2)) when h1 = h2 -> equal t1 t2
    | _ -> false
  ;;
end

let stream = Alcotest.testable Stream_test.pp Stream_test.equal

let test_plusplus () =
  let open Stream in
  Alcotest.(check stream) "empty ++ empy" Nil (Nil ++ Nil);
  Alcotest.(check stream)
    "non-empty ++ empty"
    (Cons (lazy ("a", Nil)))
    (Cons (lazy ("a", Nil)) ++ Nil);
  Alcotest.(check stream)
    "non-empty ++ non-empty"
    (Cons (lazy ("a", Cons (lazy ("b", Nil)))))
    (Cons (lazy ("a", Nil)) ++ Cons (lazy ("b", Nil)))
;;

let test_take () =
  let open Stream in
  Alcotest.(check stream) "take zero empty" Nil (take 0 Nil);
  Alcotest.(check stream) "take one empty" Nil (take 1 Nil);
  Alcotest.(check stream)
    "take zero non-empty"
    Nil
    (take 0 (Cons (lazy ("1", Cons (lazy ("2", Nil))))));
  Alcotest.(check stream)
    "take one non-empty"
    (Cons (lazy ("1", Nil)))
    (take 1 (Cons (lazy ("1", Cons (lazy ("2", Nil))))));
  Alcotest.(check stream)
    "take two non-empty"
    (Cons (lazy ("1", Cons (lazy ("2", Nil)))))
    (take 2 (Cons (lazy ("1", Cons (lazy ("2", Nil))))))
;;

let test_drop () =
  let open Stream in
  Alcotest.(check stream) "drop zero empty" Nil (drop 1 Nil);
  Alcotest.(check stream) "drop one empty" Nil (drop 1 Nil);
  Alcotest.(check stream)
    "drop zero non-empty"
    (Cons (lazy ("1", Cons (lazy ("2", Nil)))))
    (drop 0 (Cons (lazy ("1", Cons (lazy ("2", Nil))))));
  Alcotest.(check stream)
    "drop one non-empty"
    (Cons (lazy ("2", Nil)))
    (drop 1 (Cons (lazy ("1", Cons (lazy ("2", Nil))))));
  Alcotest.(check stream)
    "drop two non-empty"
    Nil
    (drop 2 (Cons (lazy ("1", Cons (lazy ("2", Nil))))))
;;

let test_reverse () =
  let open Stream in
  Alcotest.(check stream) "reverse empty" Nil (reverse Nil);
  Alcotest.(check stream)
    "reverse non-empty"
    (Cons (lazy ("2", Cons (lazy ("1", Nil)))))
    (reverse (Cons (lazy ("1", Cons (lazy ("2", Nil))))))
;;

(* Stream_fn *)

module Stream_fn_test = struct
  let rec pp_fn fmt = function
    | Stream_fn.Empty -> Format.fprintf fmt "Empty"
    | Stream_fn.Cons { head; tail } ->
      Format.fprintf
        fmt
        "Cons { head = (fun () -> %d) ; tail = (fun () -> %a) }"
        (head ())
        (fun fmt -> pp_fn fmt)
        (tail ())
  ;;

  let rec equal t1 t2 =
    let open Stream_fn in
    match t1, t2 with
    | Empty, Empty -> true
    | Cons s1, Cons s2 when s1.head () = s2.head () -> equal (s1.tail ()) (s2.tail ())
    | _ -> false
  ;;
end

let stream_fn = Alcotest.testable Stream_fn_test.pp_fn Stream_fn_test.equal

let test_plusplus_fn () =
  let open Stream_fn in
  Alcotest.(check stream_fn) "empty ++ empty" Empty (Empty ++ Empty);
  Alcotest.(check stream_fn)
    "non-empty ++ empty"
    (Cons { head = (fun () -> 9); tail = (fun () -> Empty) })
    (Cons { head = (fun () -> 9); tail = (fun () -> Empty) } ++ Empty)
;;

let test_take_fn () =
  let open Stream_fn in
  Alcotest.(check stream_fn) "take zero from empty" Empty (take 0 Empty);
  Alcotest.(check stream_fn) "take one from empty" Empty (take 1 Empty);
  Alcotest.(check stream_fn)
    "take zero from non-empty"
    Empty
    (take
       0
       (Cons
          { head = (fun () -> 1)
          ; tail = (fun () -> Cons { head = (fun () -> 2); tail = (fun () -> Empty) })
          }));
  Alcotest.(check stream_fn)
    "take one from non-empty"
    (Cons { head = (fun () -> 1); tail = (fun () -> Empty) })
    (take
       1
       (Cons
          { head = (fun () -> 1)
          ; tail = (fun () -> Cons { head = (fun () -> 2); tail = (fun () -> Empty) })
          }));
  Alcotest.(check stream_fn)
    "take two from non-empty"
    (Cons
       { head = (fun () -> 1)
       ; tail = (fun () -> Cons { head = (fun () -> 2); tail = (fun () -> Empty) })
       })
    (take
       2
       (Cons
          { head = (fun () -> 1)
          ; tail = (fun () -> Cons { head = (fun () -> 2); tail = (fun () -> Empty) })
          }))
;;

let test_drop_fn () =
  let open Stream_fn in
  Alcotest.(check stream_fn) "drop 0 from empty" Empty (drop 0 Empty);
  Alcotest.(check stream_fn) "drop 1 from empty" Empty (drop 1 Empty);
  Alcotest.(check stream_fn)
    "drop 0 from empty"
    (Cons
       { head = (fun () -> 1)
       ; tail = (fun () -> Cons { head = (fun () -> 2); tail = (fun () -> Empty) })
       })
    (drop
       0
       (Cons
          { head = (fun () -> 1)
          ; tail = (fun () -> Cons { head = (fun () -> 2); tail = (fun () -> Empty) })
          }));
  Alcotest.(check stream_fn)
    "drop 1 from empty"
    (Cons { head = (fun () -> 2); tail = (fun () -> Empty) })
    (drop
       1
       (Cons
          { head = (fun () -> 1)
          ; tail = (fun () -> Cons { head = (fun () -> 2); tail = (fun () -> Empty) })
          }));
  Alcotest.(check stream_fn)
    "drop 2 from empty"
    Empty
    (drop
       2
       (Cons
          { head = (fun () -> 1)
          ; tail = (fun () -> Cons { head = (fun () -> 2); tail = (fun () -> Empty) })
          }))
;;

let test_reverse_fn () =
  let open Stream_fn in
  Alcotest.(check stream_fn) "reverse empty" Empty (reverse Empty);
  Alcotest.(check stream_fn)
    "reverse non-empty"
    (Cons
       { head = (fun () -> 2)
       ; tail = (fun () -> Cons { head = (fun () -> 1); tail = (fun () -> Empty) })
       })
    (reverse
       (Cons
          { head = (fun () -> 1)
          ; tail = (fun () -> Cons { head = (fun () -> 2); tail = (fun () -> Empty) })
          }))
;;

let suite =
  [ "Stream.(++)", `Quick, test_plusplus
  ; "Stream.take", `Quick, test_take
  ; "Stream.drop", `Quick, test_drop
  ; "Stream.reverse", `Quick, test_reverse
  ; "Stream_fn.(++)", `Quick, test_plusplus_fn
  ; "Stream_fn.take", `Quick, test_take_fn
  ; "Stream_fn.drop", `Quick, test_drop_fn
  ; "Stream_fn.reverse", `Quick, test_reverse_fn
  ]
;;
