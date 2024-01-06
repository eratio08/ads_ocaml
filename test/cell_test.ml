open Ads_ocaml.Cell

let test_make () =
  let open ArrayCell in
  Alcotest.(check string) "should make cell" "[|1|]" (show Fmt.int @@ make 1)
;;

let test_get () =
  let open ArrayCell in
  Alcotest.(check int) "should get cell value" 1 (make 1 |> get)
;;

let test_set () =
  let open ArrayCell in
  Alcotest.(check string)
    "should set cell value"
    "[|3|]"
    (make 1
     |> fun cell ->
     set cell 3;
     cell |> show Fmt.int)
;;

let suite =
  [ "ArrayCell.make", `Quick, test_make
  ; "ArrayCell.get", `Quick, test_get
  ; "ArrayCell.set", `Quick, test_set
  ]
;;
