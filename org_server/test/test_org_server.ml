[@@@ocaml.warning "-26"]  (* Disable unused-value-declaration warnings *)
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
  let truncate_table table =
    let query = "TRUNCATE TABLE " ^ table ^ " CASCADE;" in
    ignore(Org_lib.Db.query_db query ());
    (* match Org_lib.Db.query_db query () with *)
    (* | Ok _ -> *) (*   Lwt_io.printf "Query successful for table: %s\n" table *)
    (* | Error err -> *)
    (*   Lwt_io.printf "Query failed for table: %s with error: %s\n" table err *)
    Lwt.return_unit
  in
  let tables = ["tags"; "things"; "tags_to_things"; "time_blocks"] in
  Lwt_list.iter_s truncate_table tables >>= fun () ->
  (* Lwt_io.printf "Alllll tables cleared successfully.\n" >>= fun () -> *)
  Lwt.return_unit


let get_things_from_api () =
  let uri = Uri.of_string "http://localhost:7777/things" in
  Cohttp_lwt_unix.Client.get uri

let get_tags_from_api () =
  let uri = Uri.of_string "http://localhost:7777/tags" in
  Cohttp_lwt_unix.Client.get uri

let get_thing_from_api thing_id =
  let uri = Uri.of_string (Printf.sprintf "http://localhost:7777/things/%d" thing_id) in
  Cohttp_lwt_unix.Client.get uri

let remove_tag_from_thing_in_api thing_id tag_id =
  let uri = Uri.of_string (
      Printf.sprintf "http://localhost:7777/things/%d/tags/%d" thing_id tag_id) in
  Cohttp_lwt_unix.Client.delete uri

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


let tag_thing_in_api ~(tag_id: int) ~(thing_id: int) =
  let uri = Uri.of_string "http://localhost:7777/tag-to-thing" in
  let json_payload_obj: Yojson.Basic.t =
    `Assoc [
      ("thing_id", `Int thing_id);
      ("tag_id", `Int tag_id);
    ] in
  let json_payload: string = Yojson.Basic.to_string json_payload_obj in
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

let test_tag_thing () =
  (* *** CREATE THING *** *)
  post_thing_to_api { name="thing name"; text = Some "thing text" }
  >>= fun (resp, body) ->
  parse_payload body Org_lib.Models.thing_of_yojson
  >>= fun created_thing_payload -> 
  let created_thing = parse_option_or_fail_test created_thing_payload.data in
  let created_thing_id = created_thing.id in

  (* *** CREATE TAG *** *)
  post_tag_to_api { name="tag name"; text = Some "tag text" }
  >>= fun (resp, body) ->
  parse_payload body Org_lib.Models.tag_of_yojson
  >>= fun created_tag_payload ->
  let created_tag: Org_lib.Models.tag = parse_option_or_fail_test created_tag_payload.data in
  let created_tag_id = created_tag.id in

  (* *** TAG THING *** *)
  tag_thing_in_api ~tag_id:created_tag_id ~thing_id:created_thing_id
  >>= fun (resp, body) ->
  assert_http_status resp 201;
  parse_payload body Org_lib.Models.tag_to_thing_of_yojson
  >>= fun created_tag_to_thing_payload ->
  let created_tag_to_thing = parse_option_or_fail_test created_tag_to_thing_payload.data in
  Alcotest.(check int) "tag_to_thing.tag_id should be right" created_tag_id created_tag_to_thing.tag_id;
  Alcotest.(check int) "tag_to_thing.thing_id should be right" created_thing_id created_tag_to_thing.thing_id;

  (* *** MAKE SURE TAG IS RETURNED WITH THING *** *)
  get_thing_from_api created_thing_id
  >>= fun (resp, body) ->
  assert_http_status resp 200;
  parse_payload body Org_lib.Models.thing_of_yojson
  >>= fun get_thing_payload ->
  assert_payload_success get_thing_payload;
  let thing: Org_lib.Models.thing = parse_option_or_fail_test get_thing_payload.data in
  Alcotest.(check int) "there should only be 1 tag" 1 (List.length thing.tags);
  let tag_thing_tagged_with = List.hd thing.tags in 
  Alcotest.(check int) "tag should have right id"
    created_tag.id tag_thing_tagged_with.id;
  Alcotest.(check string) "tag should have right name"
    created_tag.name tag_thing_tagged_with.name;
  Alcotest.(check (option string)) "tag should have right text"
    created_tag.text tag_thing_tagged_with.text;
  Lwt.return_unit


let test_remove_tag_from_thing () =
  (* *** CREATE THING *** *)
  post_thing_to_api { name="thing name"; text = Some "thing text" }
  >>= fun (resp, body) ->
  parse_payload body Org_lib.Models.thing_of_yojson
  >>= fun created_thing_payload -> 
  let created_thing_id = (parse_option_or_fail_test created_thing_payload.data).id in

  (* *** CREATE 2 TAGS *** *)
  post_tag_to_api { name="first tag"; text = Some "tag text" }
  >>= fun (resp, body) ->
  parse_payload body Org_lib.Models.tag_of_yojson
  >>= fun first_tag_payload ->
  let created_first_tag_id = (parse_option_or_fail_test first_tag_payload.data).id in

  let second_tag_name = "second tag" in
  post_tag_to_api { name=second_tag_name; text = Some "tag text" }
  >>= fun (resp, body) ->
  parse_payload body Org_lib.Models.tag_of_yojson
  >>= fun second_tag_payload ->
  let created_second_tag_id = (parse_option_or_fail_test second_tag_payload.data).id in


  (* *** TAG THING WITH BOTH TAGS *** *)
  tag_thing_in_api ~tag_id:created_first_tag_id ~thing_id:created_thing_id
  >>= fun (resp, body) ->
  tag_thing_in_api ~tag_id:created_second_tag_id ~thing_id:created_thing_id
  >>= fun (resp, body) ->

  (* *** ASSERT BOTH ARE ON THING *** *)
  get_thing_from_api created_thing_id
  >>= fun (resp, body) ->
  assert_http_status resp 200;
  parse_payload body Org_lib.Models.thing_of_yojson
  >>= fun get_thing_payload ->
  assert_payload_success get_thing_payload;
  let thing: Org_lib.Models.thing = parse_option_or_fail_test get_thing_payload.data in
  Alcotest.(check int) "there should be 2 tags on thing" 2 (List.length (thing.tags));

  (* *** REMOVE FIRST TAG *** *)
  remove_tag_from_thing_in_api created_thing_id created_first_tag_id
  >>= fun (resp, body) ->
  assert_http_status resp 200;
  parse_payload body Org_lib.Models.tag_of_yojson
  >>= fun remove_tag_payload ->
  assert_payload_success remove_tag_payload;

  (* *** MAKE SURE ONLY SECOND TAG IS RETURNED WITH THING *** *)
  get_thing_from_api created_thing_id
  >>= fun (resp, body) ->
  assert_http_status resp 200;
  parse_payload body Org_lib.Models.thing_of_yojson
  >>= fun get_thing_payload ->
  assert_payload_success get_thing_payload;
  let thing: Org_lib.Models.thing = parse_option_or_fail_test get_thing_payload.data in
  Alcotest.(check int) "there should only be 1 tag" 1 (List.length thing.tags);
  let remaining_tag = List.hd thing.tags in 
  Alcotest.(check int) "remaining tag should have right id"
    created_second_tag_id remaining_tag.id;
  Alcotest.(check string) "remaining tag should have right name"
    second_tag_name remaining_tag.name;
  Lwt.return_unit


let () =
  Printf.printf "wtf in main function\n\n";  (* Synchronous log *)
  let open Alcotest_lwt in
  Lwt_main.run (
    run "Org Server Tests" [
      "Ping Endpoint", [
        test_case "Ping Test" `Quick (
          fun _switch () ->
            reset_db () >>= fun () ->
            test_ping ()
        );
      ];
      "Create and get things", [
        test_case "Create and get things Test" `Quick (
          fun _switch () ->
            Lwt_io.flush Lwt_io.stdout >>= fun () ->
            reset_db () >>= fun () ->
            test_create_and_get_thing_and_tag ()
        );
      ];
      "Tag things", [
        test_case "Should be able to create a thing and a tag and tag the thing" `Quick (
          fun _switch () ->
            Lwt_io.flush Lwt_io.stdout >>= fun () ->
            reset_db () >>= fun () ->
            test_tag_thing ()
        );
      ];
      "Remove tags from things", [
        test_case "Should be able to tag a things with multiple tags, then delete only one of them" `Quick (
          fun _switch () ->
            reset_db () >>= fun () ->
            test_remove_tag_from_thing ()
        );
      ];
    ]
  )
