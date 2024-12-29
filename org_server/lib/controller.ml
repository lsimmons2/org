[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]




open Lwt.Infix
open Cohttp_lwt_unix



(* NB: function was originally called list_to_yojson, but renamed since it's more general *)
let list_to_something map_func lst = `List (List.map map_func lst)

type get_endpoint = unit -> Cohttp_lwt_unix.Server.response Lwt.t
type post_endpoint = Cohttp_lwt.Body.t -> Cohttp_lwt_unix.Server.response Lwt.t




let gen_api_resp_str success message data to_json=
  let api_resp = {
    Models.success = success;
    message = message;
    data = data;
  } in
  Yojson.Safe.to_string (Models.get_response_to_yojson to_json api_resp)


let generate_get_endpoint
    (to_json: 'b -> Yojson.Safe.t) (controller: unit -> ('b, string) result Lwt.t)
  : get_endpoint
  = fun () ->
    controller () >>= (function
        | Ok rv ->
          let json_str = gen_api_resp_str true "gucci" (Some rv) to_json in
          Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:json_str ()
        | Error err ->
          let json_str = gen_api_resp_str false "my bad" None to_json in
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


let get_things_endpoint: get_endpoint
  = generate_get_endpoint
    (list_to_something Models.thing_to_yojson)
    (fun () ->
       Lwt.return (Repository.get_things ()))


let get_tags_endpoint : get_endpoint
  = generate_get_endpoint
    (list_to_something Models.tag_to_yojson)
    (fun () -> 
       Lwt.return (Repository.get_tags ())
    )


let create_thing_endpoint : post_endpoint
  = generate_post_endpoint
    Models.create_thing_body_of_yojson
    Models.thing_to_yojson
    (fun thing_body ->
       let thing_name = thing_body.Models.name in
       let text = thing_body.Models.text in
       Lwt.return (Repository.create_thing ~thing_name ~text))


let tag_thing_endpoint : post_endpoint
  = generate_post_endpoint
    Models.tag_thing_body_of_yojson
    Models.tag_to_thing_to_yojson
    (fun tag_to_thing_body ->
       let tag_id = tag_to_thing_body.tag_id in
       let thing_id = tag_to_thing_body.thing_id in
       Lwt.return (Repository.tag_thing ~tag_id ~thing_id)
    )

