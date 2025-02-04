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

  Logger.info_lwt "%s %s" method_str path


let router _conn req body =
  let uri = Request.uri req in
  let path = Uri.path uri in

  log_request req >>= fun () ->
  Lwt.catch
    (fun () ->
       match (Request.meth req, path) with
       | (`GET, "/ping") -> Server.respond_string ~status:`OK ~body:("pong") ()

       | (`GET, "/things") -> Org_lib.Controller.get_things_endpoint uri
       | (`GET, path) when Re.execp Controller.thing_available_tags_regex path ->
         Org_lib.Controller.get_things_available_tags_endpoint uri
       | (`POST, "/things") -> Org_lib.Controller.create_thing_endpoint body
       | (`GET, path) when Re.execp Controller.things_id_regex path ->
         Org_lib.Controller.get_thing_endpoint uri
       | (`PUT, path) when Re.execp Controller.things_id_regex path ->
         Org_lib.Controller.update_thing_endpoint uri body
       | (`DELETE, path) when Re.execp Controller.things_id_regex path ->
         Org_lib.Controller.delete_thing_endpoint uri


       | (`GET, "/tags") -> Org_lib.Controller.get_tags_endpoint uri
       | (`POST, "/tags") -> Org_lib.Controller.create_tag_endpoint body
       | (`GET, path) when Re.execp Controller.tag_id_regex path ->
         Org_lib.Controller.get_tag_endpoint uri
       | (`DELETE, path) when Re.execp Controller.tag_id_regex path ->
         Org_lib.Controller.delete_tag_endpoint uri

       | (`POST, "/tag-to-thing") -> Org_lib.Controller.tag_thing_endpoint body
       | (`DELETE, path) when Re.execp Controller.untag_thing_regex path ->
         Org_lib.Controller.untag_thing_endpoint uri


       | (`POST, "/sets") -> Org_lib.Controller.create_set_endpoint body
       | (`GET, "/sets") -> Org_lib.Controller.get_sets_endpoint uri
       | (`GET, path) when Re.execp Controller.specific_set_path_regex path ->
         Org_lib.Controller.get_set_endpoint uri


       | (`GET, path) when Re.execp Controller.set_available_tags_regex path ->
         Org_lib.Controller.get_set_available_tags_endpoint uri

       | (`PUT, path) when Re.execp Controller.specific_set_path_regex path ->
         Org_lib.Controller.update_set_endpoint uri body
       | (`DELETE, path) when Re.execp Controller.specific_set_path_regex path -> Org_lib.Controller.delete_set_endpoint uri

       | (`GET, "/goto-candidates") -> Org_lib.Controller.get_goto_candidates_endpoint uri

       | _ -> (
           Logger.error_lwt "404 not found!\n" >>= fun () ->
           Server.respond_string ~status:`Not_found ~body:"Not Found" ()
         )
    )
    (fun exn ->
       (* Log the exception *)
       let error_message = Printexc.to_string exn in
       Logger.error_lwt "Uncaught exception while processing request: %s\n%!" error_message >>= fun () ->
       (* Respond with a 500 error *)
       Server.respond_string ~status:`Internal_server_error ~body:"Internal Server Error" ())

let server =
  Server.create ~mode:(`TCP (`Port 7777)) (Server.make ~callback:router ())

let () =
  let current_env = Env.current_environment () in
  Logger.info "Starting server on port %d in %s env..." 7777 (Env.string_of_env current_env);
  Lwt_main.run server
