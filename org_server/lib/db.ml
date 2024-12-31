[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]



open Lwt.Infix

let db_connect env =
  let (host, port, dbname, user, password) =
    match env with
    | `Test -> ("127.0.0.1", "5433", "org_test", "leo",  "leo" )
    | `Dev -> ( "127.0.0.1", "5433", "org", "leo", "leo" )
  in
  new Postgresql.connection ~host:host ~port:port ~dbname:dbname ~user:user ~password:password ()


let query_db (query: string) ?(params=[||]) () =
  let conn = db_connect `Test in
  let cleanup () = conn#finish in
  try
    let timestamp = string_of_float (Unix.gettimeofday ()) in
    let statement_name = Printf.sprintf "stmt_%d_%s" (Hashtbl.hash query) timestamp in
    (* Printf.printf "Preparing statement: %s with query: %s\n" statement_name query; *)
    ignore(conn#prepare statement_name query);
    (* Printf.printf "Executing prepared statement: %s\n" statement_name; *)
    let result = conn#exec_prepared ~params statement_name in
    cleanup ();
    (* Printf.printf "Query executed successfully.\n"; *)
    Ok result
  with
  | Postgresql.Error err ->
    let err_msg = Printf.sprintf "PostgreSQL error: %s" (Postgresql.string_of_error err) in
    cleanup ();
    Printf.printf "Error during query execution: %s\n" err_msg;
    Error err_msg
  | exn ->
    let err_msg = Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn) in
    cleanup ();
    Printf.printf "Unexpected exception: %s\n" err_msg;
    Error err_msg


let query_and_map
    ~(query:string)
    ~(params: string array)
    ~(mapper: Postgresql.result -> int -> 'a)
  : ('a list, string) result =
  match query_db query ~params () with
  | Ok result ->
    let rows = List.init result#ntuples (fun row -> mapper result row) in
    Ok rows
  | Error err ->
    Error err

let query_and_map_single
    ~(query: string)
    ~(params: string array)
    ~(mapper: Postgresql.result -> int -> 'a)
  : ('a, string) result =
  match query_and_map ~query ~params ~mapper with
  | Ok rows ->
    (match rows with
     | [row] -> Ok row
     | [] -> Error "No rows returned"
     | _ -> Error "Unexpected number of rows returned")
  | Error err -> Error err
