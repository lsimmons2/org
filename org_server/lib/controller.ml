[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]


(* /things/:id *)
let things_id_regex = Re.Pcre.regexp "^/things/([^/]+)$"
let thing_available_tags_regex = Re.Pcre.regexp "^/things/([0-9a-zA-Z_-]+)/available-tags$"

let tag_id_regex = Re.Pcre.regexp "^/tags/([^/]+)$"
(* /things/:id/tags/:id *)
let untag_thing_regex = Re.Pcre.regexp "^/things/([^/]+)/tags/([^/]+)$"
(* /sets/:id *)
let specific_set_path_regex = Re.Pcre.regexp "^/sets/([^/]+)$"
let set_available_tags_regex = Re.Pcre.regexp "^/sets/([0-9a-zA-Z_-]+)/available-tags$"


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
  | Some value -> (
      try
        Some (int_of_string value)
      with
        Failure _ -> None
    )

let get_query_param_string (uri : Uri.t) (param_name : string) : string option =
  Uri.get_query_param uri param_name


(* NB: function was originally called list_to_yojson, but renamed since it's more general *)
let list_to_something map_func lst = `List (List.map map_func lst)


type get_endpoint = Uri.t -> Cohttp_lwt_unix.Server.response Lwt.t
type post_endpoint = Cohttp_lwt.Body.t -> Cohttp_lwt_unix.Server.response Lwt.t
type put_endpoint = Uri.t -> Cohttp_lwt.Body.t -> Cohttp_lwt_unix.Server.response Lwt.t
type delete_endpoint = Uri.t -> Cohttp_lwt_unix.Server.response Lwt.t


let gen_api_resp_json success message data to_json=
  let api_resp = {
    Models.success = success;
    message = message;
    data = data;
  } in
  Models.get_response_to_yojson to_json api_resp


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
    Logger.debug_lwt "Before calling controller function for path %s" (Uri.path uri) >>= fun () ->
    controller uri >>= (function
        | Ok rv ->
          let resp_json = gen_api_resp_json true "gucci" (Some rv) to_json in
          Logger.debug_lwt "returning data %s" (Yojson.Safe.pretty_to_string resp_json) >>= fun () ->
          Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:(Yojson.Safe.to_string resp_json) ()
        | Error err ->
          Logger.error_lwt "Error in endpoint %s\n" err >>= fun () ->
          let resp_json = gen_api_resp_json false "my bad" None to_json in
          let json_str = Yojson.Safe.to_string resp_json in
          Cohttp_lwt_unix.Server.respond_string ~status:`Internal_server_error ~body:json_str ())


let generate_delete_endpoint
    (controller: Uri.t -> (unit, string) result Lwt.t)
  : delete_endpoint
  = fun uri ->
    controller uri >>= (function
        | Ok rv ->
          let json_str = gen_api_resp_str_no_data true "gucci deleted" in
          Logger.debug_lwt "returning data %s\n" json_str >>= fun () ->
          Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:json_str ()
        | Error err ->
          Logger.error_lwt "Error in endpoint %s\n" err >>= fun () ->
          let json_str = gen_api_resp_str_no_data false "my bad" in
          Cohttp_lwt_unix.Server.respond_string ~status:`Internal_server_error ~body:json_str ())


(* TODO: wrote this function to avoid "pattern matching/try-catch hell" in generate_post_endpoint - might be able to do this better *)
let parse_json
    json_str
    of_json = 
  try Yojson.Safe.from_string json_str |> of_json
  with Yojson.Json_error msg -> Error msg


let generate_post_endpoint
    (of_json: Yojson.Safe.t -> ('a, string) result)
    (to_json: 'b -> Yojson.Safe.t)
    (controller: 'a -> ('b, string) result Lwt.t)
  : post_endpoint
  = fun (body: Cohttp_lwt.Body.t) ->

    Cohttp_lwt.Body.to_string body >>= fun body_str ->
    match parse_json body_str of_json with
    | Ok parsed_body -> 
      controller parsed_body >>= (function
          | Ok rv ->
            let resp_json = gen_api_resp_json true "gucci" (Some rv) to_json in
            Logger.debug_lwt "Returning data from endpoint: %s\n" (Yojson.Safe.pretty_to_string resp_json) >>= fun () ->
            Cohttp_lwt_unix.Server.respond_string ~status:`Created ~body:(Yojson.Safe.to_string resp_json) ()
          | Error err ->
            Logger.error_lwt "Error in controller function: %s\n" err >>= fun () ->
            let resp_json = gen_api_resp_json false "my bad" None to_json in
            Cohttp_lwt_unix.Server.respond_string ~status:`Internal_server_error ~body:(Yojson.Safe.to_string resp_json) ())
    | Error json_body_parse_err ->
      let resp_json = gen_api_resp_json false "nah babi - bad request" None to_json in
      Logger.error_lwt "Bad request! this body doesn't look right: %s\n" body_str >>= fun () ->
      Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request ~body:(Yojson.Safe.to_string resp_json) ()



let generate_put_endpoint
    (of_json: Yojson.Safe.t -> ('a, string) result)
    (to_json: 'b -> Yojson.Safe.t)
    (controller: Uri.t -> 'a -> ('b, string) result Lwt.t)
  : put_endpoint
  = fun (uri: Uri.t) (body: Cohttp_lwt.Body.t) ->
    Cohttp_lwt.Body.to_string body >>= fun body_str ->

    Logger.debug_lwt "Body passed: %s\n" body_str >>= fun () ->

    match Yojson.Safe.from_string body_str |> of_json with
    | Ok parsed_body -> 

      controller uri parsed_body >>= (function
          | Ok rv ->
            let resp_json = gen_api_resp_json true "gucci" (Some rv) to_json in
            Logger.debug_lwt "Returning data from endpoint: %s\n" (Yojson.Safe.pretty_to_string resp_json) >>= fun () ->
            Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:(Yojson.Safe.to_string resp_json) ()
          | Error err ->
            Logger.error_lwt "Error in controller function: %s\n" err >>= fun () ->
            let resp_json = gen_api_resp_json false "my bad" None to_json in
            Cohttp_lwt_unix.Server.respond_string ~status:`Internal_server_error ~body:(Yojson.Safe.to_string resp_json) ())
    | Error json_body_parse_err ->
      let resp_json = gen_api_resp_json false "nah babi - bad request" None to_json in
      Logger.error_lwt "Bad request! this body doesn't look right: %s\n" body_str >>= fun () ->
      Logger.error_lwt "err: %s\n" json_body_parse_err >>= fun () ->
      Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request ~body:(Yojson.Safe.to_string resp_json) ()


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
       Logger.info_lwt "Logging with info_lwt: getting thing %d" thing_id >>= fun () ->
       Lwt.return (Repository.get_thing thing_id))


let get_things_endpoint
  : get_endpoint
  = generate_get_endpoint
    (list_to_something Models.thing_to_yojson)
    (fun _uri ->
       Logger.info_lwt "Logging with info_lwt: getting all things" >>= fun () ->

       Lwt.return (Repository.get_things ()))


let get_tags_endpoint
  : get_endpoint
  = generate_get_endpoint
    (list_to_something Models.tag_to_yojson)
    (fun _uri -> 
       Lwt.return (Repository.get_tags ())
    )


let get_tag_endpoint
  : get_endpoint
  = generate_get_endpoint
    Models.tag_rest_to_yojson
    (fun uri ->
       let path = Uri.path uri in
       let matches = Re.exec tag_id_regex path in
       let tag_id_str = Re.Group.get matches 1 in
       let tag_id = int_of_string tag_id_str in
       match (Repository.get_tag tag_id) with
       | Ok tag -> (
           match Repository.get_things_for_yes_and_no_tag_ids [tag.id] [] with
           | Ok things ->
             Lwt.return_ok {
               Models.id=tag.id;
               name=tag.name;
               text=tag.text;
               things=things
             }
           | Error err -> Lwt.return_error err
         )
       | Error err -> Lwt.return_error err
    )



let create_thing_endpoint
  : post_endpoint
  = generate_post_endpoint
    Models.create_thing_body_of_yojson
    Models.thing_to_yojson
    (fun thing_body ->
       let thing_name = thing_body.Models.name in
       let text = thing_body.Models.text in
       Repository.create_thing ~thing_name ~text
    )


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


let delete_thing_endpoint
  : delete_endpoint
  = generate_delete_endpoint
    (fun uri ->

       let path = Uri.path uri in
       let matches = Re.exec things_id_regex path in
       let thing_id_str = Re.Group.get matches 1 in
       let thing_id = int_of_string thing_id_str in

       Lwt.return (Repository.delete_thing thing_id)
    )


let delete_tag_endpoint
  : delete_endpoint
  = generate_delete_endpoint
    (fun uri ->
       let path = Uri.path uri in
       let matches = Re.exec tag_id_regex path in
       let tag_id_str = Re.Group.get matches 1 in
       let tag_id = int_of_string tag_id_str in

       Lwt.return (Repository.delete_tag tag_id)
    )

let create_set_endpoint
  : post_endpoint
  = generate_post_endpoint
    Models.create_set_body_of_yojson
    Models.set_rest_to_yojson
    (fun set_body ->
       let name = set_body.Models.name in
       let text = set_body.Models.text in
       match Repository.create_set ~name ~text with
       | Ok s -> Lwt.return (Repository.set_rest_of_set s)
       | Error err -> Lwt.return_error err
    )


let get_sets_endpoint
  : get_endpoint
  = generate_get_endpoint
    (list_to_something Models.set_to_yojson)
    (fun uri ->
       Lwt.return (Repository.get_all_sets()))


let get_set_endpoint
  : get_endpoint
  = generate_get_endpoint
    Models.set_rest_to_yojson
    (fun uri ->
       let path = Uri.path uri in
       let matches = Re.exec specific_set_path_regex path in
       let set_id_str = Re.Group.get matches 1 in
       let set_id = int_of_string set_id_str in
       let set_result = Repository.get_set_rest set_id in
       Lwt.return set_result)


let delete_set_endpoint
  : delete_endpoint
  = generate_delete_endpoint
    (fun uri ->
       let path = Uri.path uri in
       let matches = Re.exec specific_set_path_regex path in
       let set_id_str = Re.Group.get matches 1 in
       let set_id = int_of_string set_id_str in
       Repository.delete_set set_id;
       Lwt.return_ok ())


let update_set_endpoint
  : put_endpoint
  = generate_put_endpoint
    Models.update_set_body_of_yojson
    Models.set_rest_to_yojson
    (fun uri set_body ->
       let path = Uri.path uri in
       let matches = Re.exec specific_set_path_regex path in
       let set_id_str = Re.Group.get matches 1 in
       let set_id = int_of_string set_id_str in

       let name = set_body.Models.name in
       let text = set_body.Models.text in
       let yes_ids_to_add = set_body.Models.yes_ids_to_add in


       let to_print =
         match yes_ids_to_add with
         | Some dese -> String.concat ", " (List.map string_of_int dese)
         | None -> "No yes_ids_to_add!"
       in

       let yes_ids_to_remove = set_body.Models.yes_ids_to_remove in
       let no_ids_to_add = set_body.Models.no_ids_to_add in
       let no_ids_to_remove = set_body.Models.no_ids_to_remove in

       let updated = Repository.update_set
           set_id
           ?name:(name)
           ?text:(text)
           ?yes_ids_to_add:(yes_ids_to_add)
           ?yes_ids_to_remove:(yes_ids_to_remove)
           ?no_ids_to_add:(no_ids_to_add)
           ?no_ids_to_remove:(no_ids_to_remove)
           ()
       in
       match updated with
       | Ok s -> Lwt.return (Repository.set_rest_of_set s)
       | Error err -> Lwt.return_error err
    )


let update_thing_endpoint
  : put_endpoint
  = generate_put_endpoint
    Models.update_thing_body_of_yojson
    Models.thing_to_yojson
    (fun uri body ->
       let path = Uri.path uri in
       let matches = Re.exec things_id_regex path in
       let thing_id_str = Re.Group.get matches 1 in
       let thing_id = int_of_string thing_id_str in

       let name = body.Models.name in
       let text = body.Models.text in
       Repository.update_thing
         thing_id
         ?name:(name)
         ?text:(text)
         ()
    )


let get_things_available_tags_endpoint
  : get_endpoint
  = generate_get_endpoint
    (list_to_something Models.tag_to_yojson)
    (fun uri ->
       let path = Uri.path uri in
       let matches = Re.exec thing_available_tags_regex path in
       let thing_id_str = Re.Group.get matches 1 in
       let thing_id = int_of_string thing_id_str in
       Lwt.return (Repository.get_tags_available_for_thing thing_id))


let get_set_available_tags_endpoint
  : get_endpoint
  = generate_get_endpoint
    (list_to_something Models.tag_to_yojson)
    (fun uri ->
       let path = Uri.path uri in
       let matches = Re.exec set_available_tags_regex path in
       let set_id_str = Re.Group.get matches 1 in
       let set_id = int_of_string set_id_str in
       Lwt.return (Repository.get_tags_available_for_set set_id))

let map_sets_to_candidates (sets: Models.set list) : Models.goto_candidate_rest list =
  List.map (fun (s: Models.set) -> { Models.entity_id = s.id; entity_name = s.name; entity_type = Set_; entity_text = s.text }) sets

let map_things_to_candidates (things: Models.thing list) : Models.goto_candidate_rest list =
  List.map (fun (t: Models.thing) -> { Models.entity_id = t.id; entity_name = t.name; entity_type = Thing; entity_text = t.text }) things

let map_tags_to_candidates (tags: Models.tag list) : Models.goto_candidate_rest list =
  List.map (fun (t: Models.tag) -> { Models.entity_id = t.id; entity_name = t.name; entity_type = Tag; entity_text = t.text }) tags

let parse_entity_type_filter uri =
  let query = Uri.query uri in
  match List.assoc_opt "type" query with
  | Some ["thing"] -> [Models.Thing]
  | Some ["tag"] -> [Tag]
  | Some ["set"] -> [Set_]
  | Some types -> 
    List.filter_map (function
        | "thing" -> Some Models.Thing
        | "tag" -> Some Tag
        | "set" -> Some Set_
        | _ -> None
      ) types
  | None -> [Thing; Tag; Set_]  (* no type query param - get sets, things, and tags *)

let get_goto_candidates_endpoint
  : get_endpoint
  = generate_get_endpoint
    (list_to_something Models.goto_candidate_rest_to_yojson)
    (fun uri ->
       let requested_types = parse_entity_type_filter uri in

       (* Only fetch the required types *)
       let sets_promise =
         if List.mem Models.Set_ requested_types then
           Lwt.return (Repository.get_all_sets ()) >|= Result.map map_sets_to_candidates
         else Lwt.return (Ok [])
       in

       let things_promise =
         if List.mem Models.Thing requested_types then
           Lwt.return (Repository.get_things ()) >|= Result.map map_things_to_candidates
         else Lwt.return (Ok [])
       in

       let tags_promise =
         if List.mem Models.Tag requested_types then
           Lwt.return (Repository.get_tags ()) >|= Result.map map_tags_to_candidates
         else Lwt.return (Ok [])
       in

       (* Run all the necessary queries concurrently *)
       Lwt.all [sets_promise; things_promise; tags_promise] >>= fun results ->
       match results with
       | [Ok sets; Ok things; Ok tags] ->
         let combined = sets @ things @ tags in
         Lwt.return (Ok combined)
       | _ ->
         Lwt.return (Error "One or more queries failed")
    )
