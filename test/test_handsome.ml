let () =
  Alcotest.run
    "handsome"
    ([ Test_doc.suite; Test_render.suite; Test_diff.suite ]
     @ Test_laws.suites
     @ Test_width.suites)
;;
