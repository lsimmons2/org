[@@@ocaml.warning "-27"]  (* Disable unused-value-declaration warnings *)
[@@@ocaml.warning "-32"]  (* Disable unused-value-declaration warnings *)
[@@@ocaml.warning "-69"]  (* Disable unused-value-declaration warnings *)
[@@@ocaml.warning "-34"]  (* Disable unused-value-declaration warnings *)

open Lwt.Infix

let test_ping () =
  let uri = Uri.of_string "http://localhost:7777/ping" in
  Cohttp_lwt_unix.Client.get uri >>= fun (resp, body) ->
  let status = Cohttp.Response.status resp in
  Alcotest.(check int) "Ping response status should be 200" 200 (Cohttp.Code.code_of_status status);
  Cohttp_lwt.Body.to_string body >>= fun body_str ->
  Alcotest.(check string) "Ping response body should be pong" "pong" body_str;
  (* failwith "durp" *)
  Lwt.return_unit


let reset_db () =
  Lwt_io.printf "\n\n\nClearing db!\n\n\n" >>= fun () ->
  let truncate_table table =
    let conn = Org_lib.Db.db_connect `Test in
    let query = "TRUNCATE TABLE " ^ table ^ " CASCADE;" in
    Lwt_io.printf "query to be run in reset_db: %s\n" query >>= fun () ->
    match Org_lib.Db.query_db conn query () with
    | Ok _ ->
      conn#finish;
      Lwt_io.printf "Query successful for table: %s\n" table
    | Error err ->
      conn#finish;
      Lwt_io.printf "Query failed for table: %s with error: %s\n" table err
  in
  let tables = ["tags"; "things"; "tags_to_things"; "time_blocks"] in
  List.fold_left
    (fun acc table -> acc >>= fun () -> truncate_table table)
    (Lwt.return_unit)
    tables
  >>= fun () ->
  Lwt.return_unit


let reset_db_hook () =
  Lwt_io.printf "holaaaaaaa in reset_db_hook\n\n" >>= fun () ->
  Lwt_io.flush Lwt_io.stdout >>= fun () ->
  reset_db ()


let get_things_from_api () =
  let uri = Uri.of_string "http://localhost:7777/things" in
  Cohttp_lwt_unix.Client.get uri

let get_tags_from_api () =
  let uri = Uri.of_string "http://localhost:7777/tags" in
  Cohttp_lwt_unix.Client.get uri

let post_thing_to_api (body:Org_lib.Models.create_thing_body) =
  let uri = Uri.of_string "http://localhost:7777/things" in
  let json_payload_obj: Yojson.Basic.t =
    `Assoc [
      ("name", `String body.Org_lib.Models.name);
      ("text", match body.Org_lib.Models.text with
        | Some t -> `String t
        | None -> `Null);
    ] in
  let json_payload:string = Yojson.Basic.to_string json_payload_obj in
  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  let body = Cohttp_lwt.Body.of_string json_payload in
  Cohttp_lwt_unix.Client.post ~headers ~body uri


let post_tag_to_api (body:Org_lib.Models.create_tag_body) =
  let uri = Uri.of_string "http://localhost:7777/tags" in
  let json_payload_obj: Yojson.Basic.t =
    `Assoc [
      ("name", `String body.name);
      ("text", match body.text with
        | Some t -> `String t
        | None -> `Null);
    ] in
  let json_payload:string = Yojson.Basic.to_string json_payload_obj in
  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  let body = Cohttp_lwt.Body.of_string json_payload in

  Cohttp_lwt_unix.Client.post ~headers ~body uri


let assert_http_status resp status =
  let message = (Printf.sprintf "Response should have status %d" status) in
  Alcotest.(check int)
    message status (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
  ()


let assert_payload_success body =
  Alcotest.(check bool)
    "success field should be true" true body.Org_lib.Models.success;
  ()



let parse_payload
    (body: Cohttp_lwt.Body.t)
    (parse_yojson_func: Yojson.Safe.t -> ('a, string) result)
  : 'a Org_lib.Models.get_response Lwt.t =
  Cohttp_lwt.Body.to_string body
  >>= fun body_str ->
  (* Lwt_io.printf "parsing http resp body: %s\n" body_str >>= fun () -> *)
  match
    Yojson.Safe.from_string body_str
    |> Org_lib.Models.get_response_of_yojson (fun json ->
        match parse_yojson_func json with
        |Ok value -> Ok value
        |Error err -> Error err)
  with
  | Ok parsed -> Lwt.return parsed
  | Error err -> Alcotest.fail (Printf.sprintf "Couldn't parse response to type:\n%s\n%s" body_str err)

let parse_option_or_fail_test some_option =
  match some_option with
  | Some rv -> rv
  | None -> Alcotest.fail "Some option value should've been here"

let parse_json_list
    (of_yojson: Yojson.Safe.t -> ('a, string) result)
    (json: Yojson.Safe.t)
  : ('a list, string) result =
  match json with
  | `List lst -> (
      let rec parse_all acc = function
        | [] -> Ok (List.rev acc) (* Reverse the accumulator to preserve original order *)
        | x :: xs -> (
            match of_yojson x with
            | Ok parsed -> parse_all (parsed :: acc) xs
            | Error err -> Error ("Error parsing list element: " ^ err)
          )
      in
      parse_all [] lst
    )
  | _ -> Error "Expected a JSON list"

let test_create_and_get_thing_and_tag () =
  (* *** CREATE THING *** *)
  let thing_name = "some thing nameeeeeeeee" in
  let thing_text = "text for some thinggggggg" in
  post_thing_to_api { name=thing_name; text = Some thing_text }
  >>= fun (resp, body) ->
  assert_http_status resp 201;
  parse_payload body Org_lib.Models.thing_of_yojson
  >>= fun created_thing_payload -> 
  assert_payload_success created_thing_payload;
  let created_thing = parse_option_or_fail_test created_thing_payload.data in
  Alcotest.(check string) "thing name should be right" thing_name created_thing.name;
  Alcotest.(check (option string))
    "thing text should be right" (Some thing_text) created_thing.text;

  (* *** CREATE TAG *** *)
  let tag_name = "some tag nameeeeeeeeee" in
  let tag_text = "text for some taggggggggg" in
  post_tag_to_api { name=tag_name; text = Some tag_text }
  >>= fun (resp, body) ->
  assert_http_status resp 201;
  parse_payload body Org_lib.Models.tag_of_yojson
  >>= fun created_tag_payload ->
  assert_payload_success created_tag_payload;
  let created_tag: Org_lib.Models.tag = parse_option_or_fail_test created_tag_payload.data in
  Alcotest.(check string) "tag name should be right" tag_name created_tag.name;
  Alcotest.(check (option string)) "tag text should be right" (Some tag_text) created_tag.text;

  (* *** GET THINGS *** *)
  get_things_from_api ()
  >>= fun (resp, body) ->
  assert_http_status resp 200;
  parse_payload body (parse_json_list Org_lib.Models.thing_of_yojson)
  >>= fun get_things_payload ->
  assert_payload_success get_things_payload;
  let things: Org_lib.Models.thing list = parse_option_or_fail_test get_things_payload.data in
  Alcotest.(check int) "there should only be 1 thing" 1 (List.length things);

  (* *** GET TAGS *** *)
  get_tags_from_api ()
  >>= fun (resp, body) ->
  assert_http_status resp 200;
  parse_payload body (parse_json_list Org_lib.Models.tag_of_yojson)
  >>= fun get_tags_payload ->
  assert_payload_success get_tags_payload;
  let tags: Org_lib.Models.tag list = parse_option_or_fail_test get_tags_payload.data in
  Alcotest.(check int) "there should only be 1 tag" 1 (List.length tags);

  Lwt.return_unit


let () =
  Printf.printf "wtf in main function\n\n";  (* Synchronous log *)
  let open Alcotest_lwt in
  Lwt_main.run (
    run "Org Server Tests" [
      (* "Ping Endpoint", [ *)
      (*   test_case "Ping Test" `Quick ( *)
      (*     fun _switch () -> *)
      (*       Lwt_io.printf "in ping test lambda thing\n" >>= fun () -> *)
      (*       Lwt_io.flush Lwt_io.stdout >>= fun () -> *)
      (*       reset_db_hook () >>= fun () -> *)
      (*       test_ping () *)
      (*   ); *)
      (* ]; *)
      "Create and get things", [
        test_case "Create and get things Test" `Quick (
          fun _switch () ->
            Lwt_io.printf "in create test\n" >>= fun () ->
            Lwt_io.flush Lwt_io.stdout >>= fun () ->
            reset_db_hook () >>= fun () ->
            test_create_and_get_thing_and_tag () >>= fun ()->
            Lwt_io.printf "\nfinished running test!!!\n\n" >>= fun () -> Lwt.return_unit
        );
      ];
    ]
  )
