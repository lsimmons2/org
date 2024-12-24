open Lwt.Infix
(* open Cohttp *)
open Cohttp_lwt_unix
(* open Postgresql *)

let db_connect () =
  new Postgresql.connection ~host:"127.0.0.1" ~port:"5433" ~dbname:"your_database" ~user:"your_username" ~password:"your_password" ()

let query_db (conn : Postgresql.connection) query ?(params=[||]) () =
  (* Prepare the query with a unique name *)
  let statement_name = "stmt" in
  ignore (conn#prepare statement_name query);
  (* Execute the prepared statement with the given params *)
  let result = conn#exec_prepared ~params statement_name in
    result

let get_things _conn _body =
  let conn: Postgresql.connection = db_connect () in
  (* No params needed for this query, so we pass an empty array *)
  let result = query_db conn "SELECT * FROM things" ~params:[||] () in
  conn#finish;
  let rows = Array.to_list (result#get_all) in
  let body =
    `Assoc [
      ("things", `List (List.map (fun row ->
        `Assoc [
          ("id", `String row.(0));
          ("name", `String row.(1))
        ]) rows))
    ]
  in
  Server.respond_string ~status:`OK ~body:(Yojson.Basic.to_string body) ()

let create_thing _conn body =
  let conn = db_connect () in
  try
    (* Parse the JSON body *)
    let json = Yojson.Basic.from_string body in
    let name = Yojson.Basic.Util.(json |> member "name" |> to_string_option) in
    let text = Yojson.Basic.Util.(json |> member "text" |> to_string_option) in
    match name, text with
    | Some name, Some text ->
      (* Insert into the database *)
      let query = "INSERT INTO things (name, text) VALUES ($1, $2)" in
      ignore (query_db conn query ~params:[|name; text|] ());
      conn#finish;
      Server.respond_string ~status:`Created ~body:"Thing created" ()
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

(* Router *)
let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.path in
    match (Request.meth req, uri) with
    | (`GET, "/ping") -> Server.respond_string ~status:`OK ~body:("pong") ()
    | (`GET, "/things") -> get_things _conn body
    | (`POST, "/things") ->
        body |> Cohttp_lwt.Body.to_string >>= create_thing _conn
    | _ -> Server.respond_string ~status:`Not_found ~body:"Not Found" ()
  in
  Server.create ~mode:(`TCP (`Port 7777)) (Server.make ~callback ())

let () =
  Printf.printf "\nHola - arrancando server on port 7777...\n%!";
  Lwt_main.run server

