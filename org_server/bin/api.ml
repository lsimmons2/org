[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]

open Lwt.Infix
open Cohttp_lwt_unix


let server =
  let router _conn req body =
    let uri = req |> Request.uri |> Uri.path in
    match (Request.meth req, uri) with
    | (`GET, "/ping") -> Server.respond_string ~status:`OK ~body:("ponggggggg") ()

    | (`GET, "/things") -> Org_lib.Controller.get_things_endpoint ()
    | (`POST, "/things") -> Org_lib.Controller.create_thing_endpoint body

    | (`GET, "/tags") -> Org_lib.Controller.get_tags_endpoint ()
    | (`POST, "/tags") -> Org_lib.Controller.create_tag_endpoint body

    | (`POST, "/tag-to-thing") -> Org_lib.Controller.tag_thing_endpoint body

    | _ -> Server.respond_string ~status:`Not_found ~body:"Not Found" ()
  in
  Server.create ~mode:(`TCP (`Port 7777)) (Server.make ~callback:router ())

let () =
  Printf.printf "\nHola - arrancando server on port 7777...\n%!";
  Lwt_main.run server

