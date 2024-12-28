open Lwt.Infix
open Cohttp_lwt_unix
[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]



let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.path in
    match (Request.meth req, uri) with
    | (`GET, "/ping") -> Server.respond_string ~status:`OK ~body:("ponggggggg") ()

    | (`GET, "/things") -> Org_lib.Controller.get_things _conn body
    | (`POST, "/things") ->
      body |> Cohttp_lwt.Body.to_string >>= Org_lib.Controller.create_thing _conn

    | (`GET, "/tags") -> Org_lib.Controller.get_tags _conn body
    | (`POST, "/tags") ->
      body |> Cohttp_lwt.Body.to_string >>= Org_lib.Controller.create_tag _conn

    | (`POST, "/tag-to-thing") ->
      body |> Cohttp_lwt.Body.to_string >>= Org_lib.Controller.tag_thing _conn

    | _ -> Server.respond_string ~status:`Not_found ~body:"Not Found" ()
  in
  Server.create ~mode:(`TCP (`Port 7777)) (Server.make ~callback ())

let () =
  Printf.printf "\nHola - arrancando server on port 7777...\n%!";
  Lwt_main.run server

