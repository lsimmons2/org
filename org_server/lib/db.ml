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
  let timestamp = string_of_float (Unix.gettimeofday ()) in
  let statement_name = Printf.sprintf "stmt_%d_%s" (Hashtbl.hash query) timestamp in
  ignore(conn#prepare statement_name query);
  let result = conn#exec_prepared ~params statement_name in
  cleanup ();
  Ok result


let query_and_map
    ~(query:string)
    ~(params: string array)
    ~(mapper: Postgresql.result -> int -> 'a)
  : ('a list, string) result =
  try
    match query_db query ~params () with
    | Ok result ->
      let rows = List.init result#ntuples (fun row -> mapper result row) in
      Ok rows
    | Error err ->
      Error err
  with
  | Postgresql.Error e ->
    Error (Printf.sprintf "PostgreSQL error: %s" (Postgresql.string_of_error e))
  (* TODO: should I just have Postgresql.Error catch block in this function? *)
  | exn -> Error (Printf.sprintf "Unexpected exception: %s" (Printexc.to_string exn))


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
