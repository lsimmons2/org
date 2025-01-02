[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]


(* /things/:id *)
let things_id_regex = Re.Pcre.regexp "^/things/([^/]+)$"
(* /things/:id/tags/:id *)
let untag_thing_regex = Re.Pcre.regexp "^/things/([^/]+)/tags/([^/]+)$"
(* /sets/:id *)
let specific_set_path_regex = Re.Pcre.regexp "^/sets/([^/]+)$"


open Lwt.Infix
open Cohttp_lwt_unix

let get_query_param_bool (uri : Uri.t) (param_name : string) : bool option =
  match Uri.get_query_param uri param_name with
  | None -> None
  | Some value -> (
      match String.lowercase_ascii value with
      | "true" -> Some true
      | "false" -> Some false
      | _ -> None
    )

let get_query_param_int (uri : Uri.t) (param_name : string) : int option =
  match Uri.get_query_param uri param_name with
  | None -> None
  | Some value -> (try Some (int_of_string value) with Failure _ -> None)

let get_query_param_string (uri : Uri.t) (param_name : string) : string option =
  Uri.get_query_param uri param_name


(* NB: function was originally called list_to_yojson, but renamed since it's more general *)
let list_to_something map_func lst = `List (List.map map_func lst)


type get_endpoint = Uri.t -> Cohttp_lwt_unix.Server.response Lwt.t
type post_endpoint = Cohttp_lwt.Body.t -> Cohttp_lwt_unix.Server.response Lwt.t
type put_endpoint = Cohttp_lwt.Body.t -> Cohttp_lwt_unix.Server.response Lwt.t
type delete_endpoint = Uri.t -> Cohttp_lwt_unix.Server.response Lwt.t


let gen_api_resp_str success message data to_json=
  let api_resp = {
    Models.success = success;
    message = message;
    data = data;
  } in
  Yojson.Safe.to_string (Models.get_response_to_yojson to_json api_resp)


let gen_api_resp_str_no_data success message =
  let api_resp = {
    Models.success = success;
    message = message;
    data = None;
  } in
  Yojson.Safe.to_string (Models.get_response_to_yojson (fun _ -> `Null) api_resp)


let generate_get_endpoint
    (to_json: 'b -> Yojson.Safe.t) (controller: Uri.t -> ('b, string) result Lwt.t)
  : get_endpoint
  = fun uri ->
    controller uri >>= (function
        | Ok rv ->
          let json_str = gen_api_resp_str true "gucci" (Some rv) to_json in
          Lwt_io.printf "returning data %s\n" json_str >>= fun () ->
          Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:json_str ()
        | Error err ->
          let json_str = gen_api_resp_str false "my bad" None to_json in
          Cohttp_lwt_unix.Server.respond_string ~status:`Internal_server_error ~body:json_str ())


let generate_delete_endpoint
    (controller: Uri.t -> (unit, string) result Lwt.t)
  : delete_endpoint
  = fun uri ->
    controller uri >>= (function
        | Ok rv ->
          let json_str = gen_api_resp_str_no_data true "gucci deleted" in
          Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:json_str ()
        | Error err ->
          let json_str = gen_api_resp_str_no_data false "my bad" in
          Cohttp_lwt_unix.Server.respond_string ~status:`Internal_server_error ~body:json_str ())


let generate_post_endpoint
    (of_json: Yojson.Safe.t -> ('a, string) result)
    (to_json: 'b -> Yojson.Safe.t)
    (controller: 'a -> ('b, string) result Lwt.t)
  : post_endpoint
  = fun (body: Cohttp_lwt.Body.t) ->
    Cohttp_lwt.Body.to_string body >>= fun body_str ->
    match Yojson.Safe.from_string body_str |> of_json with
    | Ok parsed_body -> 
      controller parsed_body >>= (function
          | Ok rv ->
            let json_str = gen_api_resp_str true "gucci" (Some rv) to_json in
            Cohttp_lwt_unix.Server.respond_string ~status:`Created ~body:json_str ()
          | Error err ->
            let json_str = gen_api_resp_str false "my bad" None to_json in
            Cohttp_lwt_unix.Server.respond_string ~status:`Internal_server_error ~body:json_str ())
    | Error json_body_parse_err ->
      let json_str = gen_api_resp_str false "nah babi - bad request" None to_json in
      Lwt_io.printf "Bad request! this body doesn't look right: %s\n" body_str >>= fun () ->
      Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request ~body:json_str ()



let generate_put_endpoint
    (of_json: Yojson.Safe.t -> ('a, string) result)
    (to_json: 'b -> Yojson.Safe.t)
    (controller: 'a -> ('b, string) result Lwt.t)
  : put_endpoint
  = fun (body: Cohttp_lwt.Body.t) ->
    Cohttp_lwt.Body.to_string body >>= fun body_str ->
    match Yojson.Safe.from_string body_str |> of_json with
    | Ok parsed_body -> 
      controller parsed_body >>= (function
          | Ok rv ->
            let json_str = gen_api_resp_str true "gucci" (Some rv) to_json in
            Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:json_str ()
          | Error err ->
            let json_str = gen_api_resp_str false "my bad" None to_json in
            Cohttp_lwt_unix.Server.respond_string ~status:`Internal_server_error ~body:json_str ())
    | Error json_body_parse_err ->
      let json_str = gen_api_resp_str false "nah babi - bad request" None to_json in
      Lwt_io.printf "Bad request! this body doesn't look right: %s\n" body_str >>= fun () ->
      Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request ~body:json_str ()


let create_tag_endpoint
  : post_endpoint
  = generate_post_endpoint
    Models.create_tag_body_of_yojson
    Models.tag_to_yojson
    (fun tag_body ->
       let name = tag_body.Models.name in
       let text = tag_body.Models.text in
       let rv: (Models.tag, string) result = Repository.create_tag ~name ~text in
       Lwt.return rv)


let get_thing_endpoint
  : get_endpoint
  = generate_get_endpoint
    Models.thing_to_yojson
    (fun uri ->
       let path = Uri.path uri in
       let matches = Re.exec things_id_regex path in
       let thing_id_str = Re.Group.get matches 1 in
       let thing_id = int_of_string thing_id_str in
       Lwt.return (Repository.get_thing thing_id))

let get_things_endpoint
  : get_endpoint
  = generate_get_endpoint
    (list_to_something Models.thing_to_yojson)
    (fun _uri ->
       Lwt.return (Repository.get_things ()))


let get_tags_endpoint
  : get_endpoint
  = generate_get_endpoint
    (list_to_something Models.tag_to_yojson)
    (fun _uri -> 
       Lwt.return (Repository.get_tags ())
    )


let create_thing_endpoint
  : post_endpoint
  = generate_post_endpoint
    Models.create_thing_body_of_yojson
    Models.thing_to_yojson
    (fun thing_body ->
       let thing_name = thing_body.Models.name in
       let text = thing_body.Models.text in
       Lwt.return (Repository.create_thing ~thing_name ~text))


let tag_thing_endpoint
  : post_endpoint
  = generate_post_endpoint
    Models.tag_thing_body_of_yojson
    Models.tag_to_thing_to_yojson
    (fun tag_to_thing_body ->
       let tag_id = tag_to_thing_body.tag_id in
       let thing_id = tag_to_thing_body.thing_id in
       Lwt.return (Repository.tag_thing ~tag_id ~thing_id)
    )


let untag_thing_endpoint
  : delete_endpoint
  = generate_delete_endpoint
    (fun uri ->
       let path = Uri.path uri in
       let matches = Re.exec untag_thing_regex path in

       let thing_id_str = Re.Group.get matches 1 in
       let thing_id = int_of_string thing_id_str in

       let tag_id_str = Re.Group.get matches 2 in
       let tag_id = int_of_string tag_id_str in

       Lwt.return (Repository.untag_thing ~tag_id ~thing_id)
    )


let create_set_endpoint
  : post_endpoint
  = generate_post_endpoint
    Models.create_set_body_of_yojson
    Models.set_to_yojson
    (fun set_body ->
       let name = set_body.Models.name in
       let text = set_body.Models.text in
       Lwt.return (Ok (Repository.create_set ~name ~text)))


let get_sets_endpoint
  : get_endpoint
  = generate_get_endpoint
    (list_to_something Models.set_to_yojson)
    (fun uri ->
       Lwt.return (Repository.get_all_sets()))


let get_set_endpoint
  : get_endpoint
  = generate_get_endpoint
    Models.set_to_yojson
    (fun uri ->
       let path = Uri.path uri in
       let matches = Re.exec specific_set_path_regex path in
       let set_id_str = Re.Group.get matches 1 in
       let set_id = int_of_string set_id_str in
       let set_result = Repository.get_set set_id in
       Lwt.return set_result)


let delete_set_endpoint
  : delete_endpoint
  = generate_delete_endpoint
    (fun uri ->
       let path = Uri.path uri in
       let matches = Re.exec untag_thing_regex path in
       let set_id_str = Re.Group.get matches 1 in
       let set_id = int_of_string set_id_str in
       Lwt.return (Repository.delete_set set_id))


let update_set_endpoint
  : put_endpoint
  = generate_put_endpoint
    Models.update_set_body_of_yojson
    Models.set_to_yojson
    (fun set_body ->
       let set_id = set_body.Models.set_id in
       let name = set_body.Models.name in
       let text = set_body.Models.text in
       let yes_ids_to_add = set_body.Models.yes_ids_to_add in
       let yes_ids_to_remove = set_body.Models.yes_ids_to_remove in
       let no_ids_to_add = set_body.Models.no_ids_to_add in
       let no_ids_to_remove = set_body.Models.no_ids_to_remove in
       let rv = Repository.update_set
           set_id
           ~name
           ~text
           ~yes_ids_to_add
           ~yes_ids_to_remove
           ~no_ids_to_add
           ~no_ids_to_remove
       in
       Lwt.return rv)
