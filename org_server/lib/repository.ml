[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]

open Lwt.Infix


let thing_mapper (result : Postgresql.result) (row : int) : Models.thing =
  {
    id = int_of_string (result#getvalue row 0);  (* Assuming "id" is in column 0 *)
    name = result#getvalue row 1;              (* Assuming "name" is in column 1 *)
    text =
      if result#nfields > 2 then
        let text_val = result#getvalue row 2 in
        if text_val = "" then None else Some text_val
      else
        None;
    (* text = (let text_val = result#getvalue row 2 in if text_val = "" then None else Some text_val); *)
    tags = [];  (* Tags will be fetched and populated later *)

  }


let tag_mapper (result : Postgresql.result) (row : int) : Models.tag =
  {
    id = int_of_string (result#getvalue row 0);  (* Assuming "id" is in column 0 *)
    name = result#getvalue row 1;              (* Assuming "name" is in column 1 *)
    text =
      if result#nfields > 2 then
        let text_val = result#getvalue row 2 in
        if text_val = "" then None else Some text_val
      else
        None;
  }


let tag_to_thing_mapper (result : Postgresql.result) (row : int) : Models.tag_to_thing =
  {
    id = int_of_string (result#getvalue row 0);
    tag_id = int_of_string (result#getvalue row 1);
    thing_id = int_of_string (result#getvalue row 2);
  }


let get_thing thing_id : (Models.thing, string) result =
  let thing_query = "SELECT id, name, text FROM things WHERE id = $1" in
  let tag_query = "SELECT tags.id, tags.name, tags.text FROM tags
                   JOIN tags_to_things ON tags.id = tags_to_things.tag_id
                   WHERE tags_to_things.thing_id = $1" in
  match Db.query_and_map_single ~query:thing_query ~params:[|string_of_int thing_id|] ~mapper:thing_mapper with
  | Ok thing ->
    (match Db.query_and_map ~query:tag_query ~params:[|string_of_int thing_id|] ~mapper:tag_mapper with
     | Ok tags ->
       Ok { thing with tags }  (* Add tags to the thing *)
     | Error err ->
       Error ("Failed to fetch tags: " ^ err))
  | Error err ->
    Error ("Failed to fetch thing: " ^ err)


(* let get_thing thing_id : (Models.thing, string) result =  *)
(*   let query = "SELECT id, name, text FROM things WHERE id = $1" in *)
(*   Db.query_and_map_single ~query:query ~params:[|string_of_int thing_id|] ~mapper:thing_mapper *)


let get_things () : (Models.thing list, string) result = 
  Db.query_and_map ~query:"SELECT * FROM things" ~params:[||] ~mapper:thing_mapper


let get_tags () : (Models.tag list, string) result =
  Db.query_and_map ~query:"SELECT * FROM tags" ~params:[||] ~mapper:tag_mapper


let get_tag tag_id : (Models.tag, string) result =
  let query = "SELECT id, name, text FROM tags WHERE id = $1" in
  Db.query_and_map_single ~query:query ~params:[|string_of_int tag_id|] ~mapper:tag_mapper


let create_thing ~thing_name ~text =
  let query = match text with
    | Some _ -> "INSERT INTO things (name, text) VALUES ($1, $2) RETURNING id, name, text"
    | None -> "INSERT INTO things (name) VALUES ($1) RETURNING id, name"
  in
  let params = match text with
    | Some t -> [|thing_name; t|]
    | None -> [|thing_name;|]
  in
  let rv = Db.query_and_map_single ~query:query ~params:params ~mapper:thing_mapper in
  Lwt.return rv


let create_tag ~name ~text =
  let query = match text with
    | Some _ -> "INSERT INTO tags (name, text) VALUES ($1, $2) RETURNING id, name, text"
    | None -> "INSERT INTO tags (name) VALUES ($1) RETURNING id, name"
  in
  let params = match text with
    | Some t -> [|name; t|]
    | None -> [|name;|]
  in
  Db.query_and_map_single ~query:query ~params:params ~mapper:tag_mapper


let tag_thing ~tag_id ~thing_id =
  let query =
    "INSERT INTO tags_to_things (tag_id, thing_id) VALUES ($1, $2) RETURNING id, tag_id, thing_id"
  in
  let params = [|(string_of_int tag_id); (string_of_int thing_id)|] in
  Db.query_and_map_single ~query:query ~params:params ~mapper:tag_to_thing_mapper


let untag_thing ~tag_id ~thing_id =
  let query =
    "DELETE FROM tags_to_things WHERE thing_id=$1 AND tag_id=$2"
  in
  let params = [|(string_of_int thing_id); (string_of_int tag_id)|] in
  match Db.query_db query ~params:params () with
  | Ok _ -> Ok ()
  | Error err -> Error err



let config = Env.get_config ()


let get_set_file_path set_id =
  let file_name = Printf.sprintf "%d.json" set_id in
  Filename.concat config.set_db_dir file_name


let extract_id fp =
  let base_name = Filename.remove_extension fp in
  (* Lwt_io.printf "base_name be %s\n" base_name >>= fun () -> *)
  int_of_string base_name


let find_highest_set_id () =
  let file_names = Sys.readdir config.set_db_dir in
  let rv = Array.fold_left(
      fun max_id file_name ->
        let file_name_id = extract_id file_name in
        if file_name_id > max_id then file_name_id else max_id
    ) 0 file_names in
  rv


let create_set ~name ~text =
  Lwt.catch
    (fun () ->
       (* Try block *)
       let new_set_id = (find_highest_set_id ()) + 1 in
       let file_path = get_set_file_path new_set_id in
       let new_set: Models.set = {
         id=new_set_id;
         name=name;
         text=text;
         yes_tag_ids=[];
         no_tag_ids=[];
         things=[];
       } in
       Yojson.Safe.to_file file_path (Models.set_to_yojson new_set);
       Lwt.return_ok new_set
    )
    (fun exn ->
       (* Catch block *)
       match exn with
       | Yojson.Json_error msg ->
         Lwt.return_error (Printf.sprintf "Failed to write JSON to file: %s" msg)
       | Sys_error msg ->
         Lwt.return_error (Printf.sprintf "Sys_error: %s" msg)
       | _ ->
         Lwt.return_error (Printf.sprintf "Unexpected exception: %s" (Printexc.to_string exn))
    )




let get_things_for_yes_and_no_tag_ids yes_tag_ids no_tag_ids =
  let int_list_to_pg_array lst =
    "{" ^ (String.concat "," (List.map string_of_int lst)) ^ "}"
  in

  let query = {|
    SELECT DISTINCT t.*
    FROM things t
    INNER JOIN tags_to_things tt ON t.id = tt.thing_id
    WHERE tt.tag_id = ANY ($1)
      AND NOT EXISTS (
        SELECT 1
        FROM tags_to_things tt2
        WHERE tt2.thing_id = t.id
          AND tt2.tag_id = ANY ($2)
      )
  |} in

  let params = [| int_list_to_pg_array yes_tag_ids;
                  int_list_to_pg_array no_tag_ids |] in
  Db.query_and_map ~query ~params ~mapper:thing_mapper




let get_set set_id: (Models.set, string) result =
  let file_path = get_set_file_path set_id in
  if Sys.file_exists file_path then
    match (Yojson.Safe.from_file file_path |> Models.set_of_yojson) with
    (* TODO: why doesn't returning error from set_of_* yojson function work? *)
    (* TODO: should this function return a result or an option? *)
    | Ok d -> (
        let things = get_things_for_yes_and_no_tag_ids d.yes_tag_ids d.no_tag_ids in
        match things with
        | Ok ts -> Ok { d with things = ts }
        | Error err -> Error err
      )
    | Error err -> Error err
  else
    Error (Printf.sprintf "no set with id %d found" set_id)


let get_all_sets () =
  try
    let file_names =
      Sys.readdir config.set_db_dir
      |> Array.to_list
      |> List.filter (fun entry ->
          let file_path = Filename.concat config.set_db_dir entry in
          Sys.file_exists file_path && not (Sys.is_directory file_path)) in

    List.fold_left (fun acc file_name ->
        let set_id_str = Filename.remove_extension file_name in
        let set_id = int_of_string set_id_str in
        match acc, (get_set set_id) with
        | Ok gucci_acc, Ok gucci_new -> Ok (gucci_new :: gucci_acc)
        | Error e, _ -> Error e
        | _, Error e -> Error e
      ) (Ok []) file_names
  with
  | Sys_error msg -> Error (Printf.sprintf "Error reading directory: %s\n" msg)


let delete_set set_id =
  let file_path = get_set_file_path set_id in
  try
    Ok (Sys.remove file_path);
  with
  | Sys_error msg -> Error ("Failed to delete set: " ^ msg)

(* SNIPPET - all this stuff *)
let add_yes_tags_to_set tag_ids set =
  let new_yes_tag_ids =
    match tag_ids with
    | Some ids -> List.fold_left 
                    (fun acc tag_id -> if List.mem tag_id acc then acc else tag_id :: acc)
                    set.Models.yes_tag_ids
                    ids
    | None -> set.yes_tag_ids
  in
  {set with yes_tag_ids = new_yes_tag_ids }


let remove_yes_tags_from_set tag_ids set =
  let new_yes_tag_ids =
    match tag_ids with
    | Some ids ->
      List.filter (fun tag_id -> not (List.mem tag_id ids)) set.Models.yes_tag_ids
    | None -> set.yes_tag_ids
  in
  {set with yes_tag_ids = new_yes_tag_ids }


let add_no_tags_to_set tag_ids set =
  let new_no_tag_ids =
    match tag_ids with
    | Some ids ->
      List.fold_left 
        (fun acc tag_id -> if List.mem tag_id acc then acc else tag_id :: acc)
        set.Models.no_tag_ids
        ids
    | None -> set.no_tag_ids
  in
  {set with no_tag_ids = new_no_tag_ids }


let remove_no_tags_from_set tag_ids set =
  let new_no_tag_ids =
    match tag_ids with
    | Some ids -> 
      List.filter (fun tag_id -> not (List.mem tag_id ids)) set.Models.no_tag_ids
    | None -> set.no_tag_ids
  in
  {set with no_tag_ids = new_no_tag_ids }


let update_set_name name (set: Models.set) =
  match name with
  | Some new_name -> { set with name = new_name }
  | None -> set


let update_set_text text (set: Models.set) =
  match text with
  | Some new_text -> { set with text = Some new_text }
  | None -> set


let update_set
    set_id
    ~name
    ~text
    ~yes_ids_to_add
    ~yes_ids_to_remove
    ~no_ids_to_add
    ~no_ids_to_remove
  =
  let file_path = get_set_file_path set_id in
  match (Yojson.Safe.from_file file_path |> Models.set_of_yojson) with
  | Ok set ->
    let updated_set =
      set
      |> update_set_name name
      |> update_set_text text
      |> add_yes_tags_to_set yes_ids_to_add
      |> remove_yes_tags_from_set yes_ids_to_remove
      |> add_no_tags_to_set no_ids_to_add
      |> remove_no_tags_from_set no_ids_to_remove
    in
    Yojson.Safe.to_file file_path (Models.set_to_yojson updated_set);
    Ok updated_set
  | Error err -> Error err
