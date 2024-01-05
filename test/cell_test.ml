open Ads_ocaml.Cell

let int_array_cell =
  let pp_int_cell ppf x = Fmt.(pf ppf "%a" (ArrayCell.pp int)) x in
  Alcotest.testable pp_int_cell ( = )
;;

let test_make () =
  Alcotest.(check int_array_cell) "make cell" (ArrayCell.make 1) (ArrayCell.make 1)
;;

let () =
  Alcotest.run "ArrayCell"
    [
      ( "make",
        [
          Alcotest.test_case "should make cell" `Quick test_make;
        ] );
    ]
