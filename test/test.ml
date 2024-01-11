let () =
  Alcotest.run
    "ADS"
    [ "Cell", Cell_test.suite
    ; "Heap", Heap_test.suite
    ; "Set", Set_test.suite
    ; "Stream", Stream_test.suite
    ]
;;
