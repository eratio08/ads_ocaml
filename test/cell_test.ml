open Ads_ocaml.Cell

let test_make () =
  Alcotest.(check string)
    "should make cell"
    "[|1|]"
    (ArrayCell.show Fmt.int @@ ArrayCell.make 1)
;;

let test_get () =
  Alcotest.(check int) "should get cell value" 1 (ArrayCell.make 1 |> ArrayCell.get)
;;

let test_set () =
  Alcotest.(check string)
    "should set cell value"
    "[|3|]"
    (ArrayCell.make 1
     |> fun cell ->
     ArrayCell.set cell 3;
     cell |> ArrayCell.show Fmt.int)
;;

let suite =
  [ "ArrayCell.make", `Quick, test_make
  ; "ArrayCell.get", `Quick, test_get
  ; "ArrayCell.set", `Quick, test_set
  ]
;;
