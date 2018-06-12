module O = Obi.OpamJsonActions

let test_owl () =
  open_in "owl-fail.json" |> Ezjsonm.from_channel |> O.installs
  |> fun actions ->
  Alcotest.(check int) "found results" (List.length actions) 8

let test_json = [("Owl fail", `Quick, test_owl)]

let () = Alcotest.run "Test Opam JSON" [("test_json", test_json)]
