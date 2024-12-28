[@@@ocaml.warning "-32"]  (* Disable unused-value-declaration warnings *)
[@@@ocaml.warning "-69"]  (* Disable unused-value-declaration warnings *)
[@@@ocaml.warning "-34"]  (* Disable unused-value-declaration warnings *)

let main = Lwt_io.printf "\nhola\n"

let im_not_a_func = Lwt.return_unit
let im_a_func () = Lwt.return_unit

let () =
  Lwt_main.run (im_a_func())
