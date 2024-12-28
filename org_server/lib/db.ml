open Lwt.Infix

let db_connect env =
  let (host, port, dbname, user, password) =
    match env with
    | `Test -> ("127.0.0.1", "5433", "org_test", "leo",  "leo" )
    | `Dev -> ( "127.0.0.1", "5433", "org", "leo", "leo" )
  in
  new Postgresql.connection ~host:host ~port:port ~dbname:dbname ~user:user ~password:password ()

(* | `Test -> new Postgresql.connection ~host:"127.0.0.1" ~port:"5433" ~dbname:"your_database" ~user:"your_username" ~password:"your_password" () *)
(* | `Dev -> new Postgresql.connection ~host:"127.0.0.1" ~port:"5433" ~dbname:"your_database" ~user:"your_username" ~password:"your_password" () *)

let query_db (conn: Postgresql.connection) (query:string) ?(params=[||]) () =
  let statement_name = "stmt" in
  ignore (conn#prepare statement_name query);
  let result = conn#exec_prepared ~params statement_name in
  result

let query_and_return_created
    (conn : Postgresql.connection)
    (query : string)
    ~(params : string array)
    ~(mapper : Postgresql.result -> int -> 'a)
  : ('a, string) result Lwt.t =
  Lwt_io.printlf "\n\nExecuting query: %s" query >>= fun () ->
  Lwt_io.printlf "With params: [%s]" (String.concat "; " (Array.to_list params)) >>= fun () ->
  let result = query_db conn query ~params () in
  Lwt_io.printlf "Query executed. Number of rows returned: %d" result#ntuples >>= fun () ->
  if result#ntuples = 0 then
    Lwt_io.printlf "No rows returned from query." >>= fun () ->
    Lwt.return (Error "No rows returned")
  else
    let row: 'a = mapper result 0 in
    (* Generic string representation for debugging *)
    (* let row_str = Printf.sprintf "%s" (Obj.magic row : string) in *)
    (* Lwt_io.printlf "First row mapped to: %s" row_str >>= fun () -> *)
    Lwt.return (Ok row)
