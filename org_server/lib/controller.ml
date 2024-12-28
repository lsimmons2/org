open Lwt.Infix
open Cohttp_lwt_unix
[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]



let gen_get_response success message data to_yojson_func =
  let resp_record = {Models.data; success; message} in
  Yojson.Safe.to_string (Models.get_response_to_yojson to_yojson_func resp_record)


(* NB: function was originally called list_to_yojson, but renamed since it's more general *)
let list_to_something map_func lst = `List (List.map map_func lst)


let get_things _conn _body =
  let things: Models.thing list = Repository.get_things () in
  let resp_json_str: string = gen_get_response
      true
      "Here are all the things"
      (Some things)
      (list_to_something Models.thing_to_yojson)
  in
  Lwt_io.printf "\nhere be the things getting returned:\n%s\n" resp_json_str >>= fun () ->
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:resp_json_str ()


let get_tags _conn _body =
  let tags = Repository.get_tags () in
  let resp_json_str = gen_get_response
      true
      "Here are all the tags" 
      (Some tags)
      (list_to_something Models.tag_to_yojson) in
  Lwt_io.printf "\nhere be the tags getting returned:\n%s\n" resp_json_str >>= fun () ->
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:resp_json_str ()



(* TODO: this could be an "('a, to_yojson_func) option" ? *)
let gen_created_response success message created to_yojson_func =
  let json_resp: Yojson.Safe.t =
    `Assoc [
      ("success", `Bool success);
      ("message", `String message);
      ("created", (
          match created with
          | Some c -> to_yojson_func c
          | None -> `Null));
    ] in
  Yojson.Safe.to_string json_resp


let tag_mapper (result : Postgresql.result) (row : int) : Models.tag =
  {
    id = int_of_string (result#getvalue row 0);  (* Assuming "id" is in column 0 *)
    name = result#getvalue row 1;              (* Assuming "name" is in column 1 *)
    text = let text_val = result#getvalue row 2 in
      if text_val = "" then None else Some text_val;  (* Assuming "text" is in column 2 *)
  }

let create_tag _conn body =
  let conn = Db.db_connect `Test in
  try
    (* Parse the JSON body *)
    let json = Yojson.Basic.from_string body in
    let name_opt = Yojson.Basic.Util.(json |> member "name" |> to_string_option) in
    let text_opt = Yojson.Basic.Util.(json |> member "text" |> to_string_option) in
    match name_opt, text_opt with
    | Some name, Some text ->
      Repository.create_tag ~name ~text
      >>= fun created_tag_result ->
      conn#finish;
      (match created_tag_result with
       | Ok created_tag -> 
         let resp_json_str: string =
           gen_created_response true "gucci created babi" (Some created_tag) Models.tag_to_yojson in
         Server.respond_string ~status:`Created ~body:resp_json_str ()
       | Error _ ->
         Server.respond_string ~status:`Not_found ~body:"woops! that wasn't supposed to happen" ())
    | _ ->
      (* Missing "name" or "text" in JSON body *)
      conn#finish;
      Server.respond_string ~status:`Bad_request ~body:"Missing 'name' or 'text' in request body" ()
  with
  | Yojson.Basic.Util.Type_error (msg, _) ->
    (* Handle malformed JSON *)
    conn#finish;
    Server.respond_string ~status:`Bad_request ~body:("Malformed JSON: " ^ msg) ()
  | exn ->
    (* Handle other unexpected errors *)
    conn#finish;
    Server.respond_string ~status:`Internal_server_error ~body:("Error: " ^ Printexc.to_string exn) ()


let respond_json ~success ~message =
  let resp_json: Yojson.Safe.t = `Assoc [("success", `Bool success); ("message", `String message)] in
  Yojson.Safe.to_string resp_json



let tag_thing _conn (body_str: string) =
  let conn = Db.db_connect `Test in
  try
    match Yojson.Safe.from_string body_str |> Models.tag_thing_body_of_yojson with
    | Ok body ->
      let query = "INSERT INTO tags_to_things (tag_id, thing_id) VALUES ($1, $2)" in
      let params = [|string_of_int body.tag_id; string_of_int body.thing_id|] in
      ignore (Db.query_db conn query ~params:params ());
      conn#finish;
      let resp_json_str = respond_json ~success:true ~message:"Tag-Thing relationship created" in
      Server.respond_string ~status:`Created ~body:resp_json_str ()
    | Error err ->
      conn#finish;
      Server.respond_string ~status:`Bad_request ~body:("Invalid JSON: " ^ err) ()
  with
  | Yojson.Basic.Util.Type_error (msg, _) ->
    (* Handle malformed JSON *)
    conn#finish;
    Server.respond_string ~status:`Bad_request ~body:("Malformed JSON: " ^ msg) ()
  | exn ->
    (* Handle other unexpected errors *)
    conn#finish;
    Server.respond_string ~status:`Internal_server_error ~body:("Error: " ^ Printexc.to_string exn) ()

let thing_mapper (result : Postgresql.result) (row : int) : Models.thing =
  {
    id = int_of_string (result#getvalue row 0);  (* Assuming "id" is in column 0 *)
    name = result#getvalue row 1;              (* Assuming "name" is in column 1 *)
    text = let text_val = result#getvalue row 2 in
      if text_val = "" then None else Some text_val;  (* Assuming "text" is in column 2 *)
  }

let create_thing _conn body =
  let conn = Db.db_connect `Test in
  try
    let json = Yojson.Basic.from_string body in
    let name = Yojson.Basic.Util.(json |> member "name" |> to_string_option) in
    let text = Yojson.Basic.Util.(json |> member "text" |> to_string_option) in
    match name, text with
    | Some name, Some text ->
      (* Insert into the database *)
      (* let query = "INSERT INTO things (name, text) VALUES ($1, $2)" in *)
      let query = "INSERT INTO things (name, text) VALUES ($1, $2) RETURNING id, name, text" in
      Db.query_and_return_created conn query ~params:[|name; text|] ~mapper:thing_mapper
      >>= fun (created_thing_result: (Models.thing, string) result) ->
      conn#finish;
      (match created_thing_result with
       | Ok created_thing -> 
         let resp_json_str: string =
           gen_created_response true "gucci created babi" (Some created_thing) Models.thing_to_yojson in
         Server.respond_string ~status:`Created ~body:resp_json_str ()
       | Error _ -> 
         Server.respond_string ~status:`Not_found ~body:"woops! that wasn't supposed to happen" ())

    | _ ->
      (* Missing "name" or "text" in JSON body *)
      conn#finish;
      Server.respond_string ~status:`Bad_request ~body:"Missing 'name' or 'text' in request body" ()
  with
  | Yojson.Basic.Util.Type_error (msg, _) ->
    (* Handle malformed JSON *)
    conn#finish;
    Server.respond_string ~status:`Bad_request ~body:("Malformed JSON: " ^ msg) ()
  | exn ->
    (* Handle other unexpected errors *)
    conn#finish;
    Server.respond_string ~status:`Internal_server_error ~body:("Error: " ^ Printexc.to_string exn) ()
