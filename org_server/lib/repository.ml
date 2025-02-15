[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]

open Lwt.Infix
open Ptime

let parse_created_at (s : string) : Ptime.t =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> t
  | Error _ -> failwith (Printf.sprintf "bad tz!!! it be %S" s)

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
    tags = [];  (* Tags will be fetched and populated later *)
    created_at =
      if result#nfields > 3 then
        parse_created_at (result#getvalue row 3)
      else
        parse_created_at (result#getvalue row 2)
  }


let tag_mapper (result : Postgresql.result) (row : int) : Models.tag =
  {
    id = int_of_string (result#getvalue row 0);  (* Assuming "id" is in column 0 *)
    name = result#getvalue row 1;              (* Assuming "name" is in column 1 *)
    text =
      if result#nfields > 3 then
        let text_val = result#getvalue row 2 in
        if text_val = "" then None else Some text_val
      else
        None;
    created_at =
      if result#nfields > 3 then
        parse_created_at (result#getvalue row 3)
      else
        parse_created_at (result#getvalue row 2)
  }


let tag_to_thing_mapper (result : Postgresql.result) (row : int) : Models.tag_to_thing =
  {
    id = int_of_string (result#getvalue row 0);
    tag_id = int_of_string (result#getvalue row 1);
    thing_id = int_of_string (result#getvalue row 2);
    created_at = parse_created_at (result#getvalue row 3);
  }


let get_thing thing_id : (Models.thing, string) result =

  let thing_query = "SELECT id, name, text, created_at FROM things WHERE id = $1" in
  let tag_query = "SELECT tags.id, tags.name, tags.text, tags.created_at FROM tags
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


let delete_thing thing_id : (unit, string) result =
  let query = "DELETE from things WHERE id = $1" in
  let params = [|string_of_int thing_id|] in
  match Db.query_db query ~params:params () with
  | Ok _ -> Ok ()
  | Error err -> Error (Printf.sprintf "this be an error: %s" err)


(* let get_thing thing_id : (Models.thing, string) result =  *)
(*   let query = "SELECT id, name, text FROM things WHERE id = $1" in *)
(*   Db.query_and_map_single ~query:query ~params:[|string_of_int thing_id|] ~mapper:thing_mapper *)


let get_things () : (Models.thing list, string) result = 
  Logger.debug "in get_things";
  Db.query_and_map ~query:"SELECT * FROM things" ~params:[||] ~mapper:thing_mapper


let get_tags () : (Models.tag list, string) result =
  Db.query_and_map ~query:"SELECT * FROM tags" ~params:[||] ~mapper:tag_mapper


let get_tag tag_id : (Models.tag, string) result =
  let query = "SELECT id, name, text, created_at FROM tags WHERE id = $1" in
  Db.query_and_map_single ~query:query ~params:[|string_of_int tag_id|] ~mapper:tag_mapper


let create_thing ~thing_name ~text =
  let query = match text with
    | Some _ -> "INSERT INTO things (name, text) VALUES ($1, $2) RETURNING id, name, text, created_at"
    | None -> "INSERT INTO things (name) VALUES ($1) RETURNING id, name, created_at"
  in
  let params = match text with
    | Some t -> [|thing_name; t|]
    | None -> [|thing_name;|]
  in
  let rv = Db.query_and_map_single ~query:query ~params:params ~mapper:thing_mapper in
  Lwt.return rv


let create_tag ~name ~text =
  let query = match text with
    | Some _ -> "INSERT INTO tags (name, text) VALUES ($1, $2) RETURNING id, name, text, created_at"
    | None -> "INSERT INTO tags (name) VALUES ($1) RETURNING id, name, created_at"
  in
  let params = match text with
    | Some t -> [|name; t|]
    | None -> [|name;|]
  in
  Db.query_and_map_single ~query:query ~params:params ~mapper:tag_mapper


let tag_thing ~tag_id ~thing_id =
  let query =
    "INSERT INTO tags_to_things (tag_id, thing_id) VALUES ($1, $2) RETURNING id, tag_id, thing_id, created_at"
  in
  let params = [|(string_of_int tag_id); (string_of_int thing_id)|] in
  Db.query_and_map_single ~query:query ~params:params ~mapper:tag_to_thing_mapper


let untag_thing ~tag_id ~thing_id =
  let query =
    "DELETE FROM tags_to_things WHERE thing_id=$1 AND tag_id=$2"
  in
  let params = [|(string_of_int thing_id); (string_of_int tag_id)|] in
  Result.bind (Db.query_db query ~params:params ()) (fun _ -> Ok ())



let config = Env.get_config ()


let get_set_file_path set_id =
  let file_name = Printf.sprintf "%d.json" set_id in
  Filename.concat config.set_db_dir file_name


let extract_id fp =
  let base_name = Filename.remove_extension fp in
  int_of_string base_name


let find_highest_set_id () =
  let file_names = Sys.readdir config.set_db_dir in
  let rv = Array.fold_left(
      fun max_id file_name ->
        let file_name_id = extract_id file_name in
        if file_name_id > max_id then file_name_id else max_id
    ) 0 file_names in
  rv


let current_timestamp_with_tz () : string =
  let now = Ptime_clock.now () in
  let tz_offset_s = (int_of_float (fst (Unix.mktime (Unix.gmtime (Unix.time ()))))) - (int_of_float ( (fst (Unix.mktime (Unix.localtime (Unix.time ())))))) in
  let tz_offset = Printf.sprintf "%+03d:%02d" (tz_offset_s / 3600) (abs (tz_offset_s mod 3600) / 60) in
  let rfc3339 = Ptime.to_rfc3339 ~frac_s:0 now in
  Printf.sprintf "%s%s" (String.sub rfc3339 0 (String.length rfc3339 - 1)) tz_offset

let current_ptime_and_tz_offset () : Ptime.t * int =
  let now = Ptime_clock.now () in
  let tz_offset_s = int_of_float (fst (Unix.mktime (Unix.gmtime (Unix.time ()))))
                    - int_of_float (fst (Unix.mktime (Unix.localtime (Unix.time ())))) in
  (now, tz_offset_s)



let create_set ~name ~text =
  let new_set_id = (find_highest_set_id ()) + 1 in
  let file_path = get_set_file_path new_set_id in
  let now = fst (current_ptime_and_tz_offset ()) in
  let new_set: Models.set = {
    id=new_set_id;
    name=name;
    text=text;
    yes_tag_ids=[];
    no_tag_ids=[];
    things=[];
    created_at=now
  } in
  Yojson.Safe.to_file file_path (Models.set_to_yojson new_set);
  Ok new_set


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


let set_rest_of_set (set: Models.set) =
  Logger.debug "in set_reset_of_set before getting things";
  let things_res = get_things_for_yes_and_no_tag_ids set.yes_tag_ids set.no_tag_ids in
  Logger.debug "in set_reset_of_set after getting things";

  let yes_tags_res =
    List.fold_left (fun acc x ->
        match acc, x with
        | Error e, _ -> Error e
        | _, Error e -> Error e
        | Ok acc_list, Ok v -> Ok (v :: acc_list)
      ) (Ok []) (List.map get_tag set.yes_tag_ids)
  in
  Logger.debug "in set_reset_of_set after getting yes tags";
  let no_tags_res =
    List.fold_left (fun acc x ->
        match acc, x with
        | Error e, _ -> Error e
        | _, Error e -> Error e
        | Ok acc_list, Ok v -> Ok (v :: acc_list)
      ) (Ok []) (List.map get_tag set.no_tag_ids)
  in
  Logger.debug "in set_reset_of_set after getting no tags";
  match things_res, yes_tags_res, no_tags_res with
  | Ok things, Ok yes_tags, Ok no_tags ->
    (Ok {
        Models.id = set.id;
        name = set.name;
        text = set.text;
        things = things;
        yes_tags = yes_tags;
        no_tags = no_tags;
        created_at = set.created_at;
      })
  | Error e, _, _ -> Error e
  | _, Error e, _ -> Error e
  | _, _, Error e -> Error e


let get_set set_id: (Models.set, string) result =
  let file_path = get_set_file_path set_id in
  if Sys.file_exists file_path then
    match (Yojson.Safe.from_file file_path |> Models.set_of_yojson) with
    | Ok d -> (
        Result.bind (get_things_for_yes_and_no_tag_ids d.yes_tag_ids d.no_tag_ids)
          (fun ts -> Ok { d with things = ts })
      )
    | Error err -> failwith (Printf.sprintf "Couldn't parse set json current ts be: " ^ current_timestamp_with_tz ())
    (* | Error err -> failwith (Printf.sprintf "Couldn't parse set json: " ^ err) *)
  else
    Error (Printf.sprintf "Set file doesn't exist: " ^ file_path)



let get_set_rest set_id: (Models.set_rest, string) result =
  let file_path = get_set_file_path set_id in
  if Sys.file_exists file_path then
    match (Yojson.Safe.from_file file_path |> Models.set_of_yojson) with
    (* TODO: why doesn't returning error from set_of_* yojson function work? *)
    (* TODO: should this function return a result or an option? *)
    | Ok d -> set_rest_of_set d
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
        | _, Error e -> Error (Printf.sprintf "Set %d not found: %s" set_id e)
      ) (Ok []) file_names
  with
  | Sys_error msg -> Error (Printf.sprintf "Error reading directory: %s\n" msg)


let delete_set set_id =
  let file_path = get_set_file_path set_id in
  Sys.remove file_path


(* SNIPPET - all this stuff *)
let add_yes_tags_to_set tag_ids set =
  let new_yes_tag_ids =
    List.fold_left 
      (fun acc tag_id -> if List.mem tag_id acc then acc else tag_id :: acc)
      set.Models.yes_tag_ids
      tag_ids
  in
  {set with yes_tag_ids = new_yes_tag_ids }


let remove_yes_tags_from_set tag_ids set =
  let new_yes_tag_ids =
    List.filter (fun tag_id -> not (List.mem tag_id tag_ids)) set.Models.yes_tag_ids
  in
  {set with yes_tag_ids = new_yes_tag_ids }


let add_no_tags_to_set tag_ids set =
  let new_no_tag_ids =
    List.fold_left 
      (fun acc tag_id -> if List.mem tag_id acc then acc else tag_id :: acc)
      set.Models.no_tag_ids
      tag_ids
  in
  {set with no_tag_ids = new_no_tag_ids }


let remove_no_tags_from_set tag_ids set =
  let first_tag_id = List.hd tag_ids in
  let new_no_tag_ids =
    List.filter (fun tag_id -> not (List.mem tag_id tag_ids)) set.Models.no_tag_ids
  in
  {set with no_tag_ids = new_no_tag_ids }


let update_set_name name (set: Models.set) =
  { set with name = name }


let update_set_text text (set: Models.set) =
  { set with text = Some text }


let update_set
    set_id
    ?name
    ?text
    ?yes_ids_to_add
    ?yes_ids_to_remove
    ?no_ids_to_add
    ?no_ids_to_remove
    ()
  =
  let file_path = get_set_file_path set_id in
  Result.bind (Yojson.Safe.from_file file_path |> Models.set_of_yojson)
    (fun set ->
       let updated_set =
         set
         |> (fun s -> match name with Some n -> update_set_name n s | None -> s)
         |> (fun s -> match text with Some t -> update_set_text t s | None -> s)
         |> (fun s -> match yes_ids_to_add with Some ids -> add_yes_tags_to_set ids s | None -> s)
         |> (fun s -> match yes_ids_to_remove with Some ids -> remove_yes_tags_from_set ids s | None -> s)
         |> (fun s -> match no_ids_to_add with Some ids -> add_no_tags_to_set ids s | None -> s)
         |> (fun s -> match no_ids_to_remove with Some ids -> remove_no_tags_from_set ids s | None -> s)
       in

       Yojson.Safe.to_file file_path (Models.set_to_yojson updated_set);
       Ok updated_set)


let update_thing thing_id ?name ?text () =

  let query =
    match (name, text) with
    | (Some _, Some _) -> "UPDATE things SET name = $1, text = $2 WHERE id = $3 RETURNING id, name, text"
    | (Some _, None) -> "UPDATE things SET name = $1 WHERE id = $2 RETURNING id, name, text"
    | (None, Some _) -> "UPDATE things SET text = $1 WHERE id = $2 RETURNING id, name, text"
    | (None, None) -> failwith "At least one of name or text must be provided"
  in
  let params =
    match (name, text) with
    | (Some n, Some t) -> [| n; t; string_of_int thing_id |]
    | (Some n, None) -> [| n; string_of_int thing_id |]
    | (None, Some t) -> [| t; string_of_int thing_id |]
    | (None, None) -> [| |]  (* This case won't occur due to failwith above *)
  in

  let rv = Db.query_and_map_single ~query:query ~params:params ~mapper:thing_mapper in
  Lwt.return rv


let remove_tag_from_sets tag_id : (unit, string) result =
  Result.bind (get_all_sets ())
    (fun sets -> (
         let results = List.map (
             fun (s: Models.set) ->
               update_set
                 s.Models.id
                 ~yes_ids_to_remove:[tag_id]
                 ~no_ids_to_remove:[tag_id]
                 ()
           ) sets in
         let errors = List.filter_map (function
             | Error e -> Some e
             | Ok _ -> None
           ) results in
         match errors with
         | [] -> Ok ()
         | errs -> Error (String.concat ", " errs)
       ))


let delete_tag tag_id =
  Result.bind (remove_tag_from_sets tag_id)
    (fun _ -> (
         let query = "DELETE FROM tags WHERE id = $1" in
         let params = [|(string_of_int tag_id)|] in
         match Db.query_db query ~params:params () with
         | Ok _ -> Ok ()
         | Error err -> Error err
       ))


let get_tags_available_for_thing thing_id =

  let query = {|
    SELECT tags.*
    FROM tags
    LEFT JOIN tags_to_things ON tags.id = tags_to_things.tag_id AND tags_to_things.thing_id = $1
    WHERE tags_to_things.tag_id IS NULL;
  |} in
  Db.query_and_map ~query:query ~params:[|(string_of_int thing_id)|] ~mapper:tag_mapper


let get_tags_available_for_set set_id =
  Result.bind (get_set set_id)
    (fun set -> (
         let used_ids = set.yes_tag_ids @ set.no_tag_ids in
         let query =
           match used_ids with
           | [] -> Printf.sprintf "SELECT * FROM tags;"
           | _ ->
             let ids_str = String.concat ", " (List.map string_of_int used_ids) in
             Printf.sprintf "SELECT * FROM tags WHERE id NOT IN (%s);" ids_str
         in

         Db.query_and_map ~query:query ~params:[||] ~mapper:tag_mapper
       ))
