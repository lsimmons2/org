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
  let conn = Org_lib.Db.db_connect `Test in
  let tables = ["things"; "tags"; "tags_to_things"; "time_blocks"] in
  List.iter (fun table ->
      let query = "TRUNCATE TABLE " ^ table ^ " CASCADE;" in
      ignore (Org_lib.Db.query_db conn query ());
    ) tables;
  conn#finish;
  Lwt.return_unit


let reset_db_hook () =
  Lwt_io.printf "holaaaaaaa in reset_db_hook\n\n" >>= fun () ->
  Lwt_io.flush Lwt_io.stdout >>= fun () ->
  reset_db ()


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


let parse_created_resp_or_fail_test
    (body: Cohttp_lwt.Body.t)
    (created_of_yojson_func: Yojson.Safe.t -> ('a, string) result)
  : 'a Org_lib.Models.created_response Lwt.t =
  Cohttp_lwt.Body.to_string body >>= fun body_str ->
  Lwt_io.printf "parsing http resp body: %s\n" body_str >>= fun () ->
  match
    Yojson.Safe.from_string body_str
    |> Org_lib.Models.created_response_of_yojson created_of_yojson_func
  with
  | Ok parsed -> Lwt.return parsed
  | Error err -> Alcotest.fail (Printf.sprintf "Couldn't parse response to type: %s" err)

let parse_option_or_fail_test some_option =
  match some_option with
  | Some rv -> rv
  | None -> Alcotest.fail "Some option value should've been here"

let test_create_and_get_thing_and_tag () =
  (* *** THING *** *)
  let thing_name = "some thing name" in
  let thing_text = "text for some thing" in
  post_thing_to_api { name=thing_name; text = Some thing_text }
  >>= fun (resp, body) ->

  let create_thing_status = Cohttp.Response.status resp in
  Alcotest.(check int)
    "Create response status should be 201" 201 (Cohttp.Code.code_of_status create_thing_status);

  parse_created_resp_or_fail_test body Org_lib.Models.thing_of_yojson
  >>= fun created_thing_resp -> 
  Alcotest.(check bool)
    "success field should be true" true created_thing_resp.success;

  let created_thing = parse_option_or_fail_test created_thing_resp.created in
  Alcotest.(check string) "thing name should be right" thing_name created_thing.name;
  Alcotest.(check (option string))
    "thing text should be right" (Some thing_text) created_thing.text;

  (* *** TAG *** *)
  let tag_name = "some tag name" in
  let tag_text = "text for some tag" in
  post_tag_to_api { name=tag_name; text = Some tag_text }
  >>= fun (resp, body) ->
  let create_tag_status = Cohttp.Response.status resp in
  Alcotest.(check int)
    "Create response status should be 201" 201 (Cohttp.Code.code_of_status create_tag_status);
  parse_created_resp_or_fail_test body Org_lib.Models.tag_of_yojson
  >>= fun created_tag_resp ->
  Alcotest.(check bool) "success field should be true" true created_tag_resp.success;
  let created_tag: Org_lib.Models.tag = parse_option_or_fail_test created_tag_resp.created in
  Alcotest.(check string) "tag name should be right" tag_name created_tag.name;
  Alcotest.(check (option string)) "tag text should be right" (Some tag_text) created_tag.text;
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
