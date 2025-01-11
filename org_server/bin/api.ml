[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]

open Lwt.Infix
open Cohttp_lwt_unix
open Org_lib



let server =
  let router _conn req body =
    let uri = Request.uri req in
    let path = Uri.path uri in
    match (Request.meth req, path) with
    | (`GET, "/ping") -> Server.respond_string ~status:`OK ~body:("pong") ()

    | (`GET, "/things") -> Org_lib.Controller.get_things_endpoint uri
    | (`POST, "/things") -> Org_lib.Controller.create_thing_endpoint body
    | (`GET, path) when Re.execp Controller.things_id_regex path ->
      Org_lib.Controller.get_thing_endpoint uri


    | (`GET, "/tags") -> Org_lib.Controller.get_tags_endpoint uri
    | (`POST, "/tags") -> Org_lib.Controller.create_tag_endpoint body
    | (`GET, path) when Re.execp Controller.tag_id_regex path ->
      Org_lib.Controller.get_tag_endpoint uri

    | (`POST, "/tag-to-thing") -> Org_lib.Controller.tag_thing_endpoint body
    | (`DELETE, path) when Re.execp Controller.untag_thing_regex path ->
      Org_lib.Controller.untag_thing_endpoint uri


    | (`POST, "/sets") -> Org_lib.Controller.create_set_endpoint body
    | (`GET, "/set") -> Org_lib.Controller.get_sets_endpoint uri
    | (`GET, path) when Re.execp Controller.specific_set_path_regex path ->
      Org_lib.Controller.get_set_endpoint uri
    | (`DELETE, path) when Re.execp Controller.specific_set_path_regex path ->
      Org_lib.Controller.delete_set_endpoint uri

    | _ -> Server.respond_string ~status:`Not_found ~body:"Not Found" ()
  in
  Server.create ~mode:(`TCP (`Port 7777)) (Server.make ~callback:router ())

let () =
  Printf.printf "\nHola - arrancando server on port 7777...\n%!";
  Lwt_main.run server
