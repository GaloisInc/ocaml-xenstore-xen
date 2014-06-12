open Mirage

let main = foreign "Unikernel.Main" (console @-> job)

let () =
  add_to_opam_packages ["xenstore-flask"];
  add_to_ocamlfind_libraries ["xenstore-flask"];
  register "xenstore" [
    main $ dummy_console
  ]

