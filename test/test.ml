let () = Alcotest.run "ADS" [ "Cell", Cell_test.suite; "Heap", Heap_test.suite ]
