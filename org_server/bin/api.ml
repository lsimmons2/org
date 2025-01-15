[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]

open Lwt.Infix
open Cohttp_lwt_unix
open Org_lib


let log_request req =
  let path = Uri.path (Request.uri req) in
  let method_str = match(Request.meth req) with
    | `GET -> "GET"
    | `POST -> "POST"
    | `PUT -> "PUT"
    | `DELETE -> "DELETE"
    | `OPTIONS -> "OPTIONS"
    | `CONNECT -> "CONNECT"
    | `TRACE -> "TRACE"
    | `HEAD -> "HEAD"
    | `PATCH -> "PATCH"
    | _ -> "*OTHER*"
  in

  Lwt_io.printf "\n%s %s\n" method_str path

let server =
  let router _conn req body =
    let uri = Request.uri req in
    let path = Uri.path uri in

    log_request req >>= fun () ->

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
    (* TODO: /sets not set *)
    | (`GET, "/set") -> Org_lib.Controller.get_sets_endpoint uri
    | (`GET, path) when Re.execp Controller.specific_set_path_regex path ->
      Org_lib.Controller.get_set_endpoint uri
    | (`PUT, path) when Re.execp Controller.specific_set_path_regex path ->
      Org_lib.Controller.update_set_endpoint uri body
    | (`DELETE, path) when Re.execp Controller.specific_set_path_regex path ->
      Org_lib.Controller.delete_set_endpoint uri

    | _ -> Server.respond_string ~status:`Not_found ~body:"Not Found" ()
  in
  Server.create ~mode:(`TCP (`Port 7777)) (Server.make ~callback:router ())

let () =
  let current_env = Env.current_environment () in
  Printf.printf "\nHola - arrancando server on port 7777 in %s env...\n%!" (Env.string_of_env current_env);
  (* Printf.printf "\nHola - arrancando server on port 7777...\n%!"; *)
  Lwt_main.run server
