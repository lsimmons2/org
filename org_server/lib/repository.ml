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
    text = (let text_val = result#getvalue row 2 in
            if text_val = "" then None else Some text_val);  (* Assuming "text" is in column 2 *)
    tags = [];  (* Tags will be fetched and populated later *)

  }


let tag_mapper (result : Postgresql.result) (row : int) : Models.tag =
  {
    id = int_of_string (result#getvalue row 0);  (* Assuming "id" is in column 0 *)
    name = result#getvalue row 1;              (* Assuming "name" is in column 1 *)
    text = let text_val = result#getvalue row 2 in
      if text_val = "" then None else Some text_val;  (* Assuming "text" is in column 2 *)
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


let create_thing ~thing_name ~text =
  let query = match text with
    | Some _ -> "INSERT INTO things (name, text) VALUES ($1, $2) RETURNING id, name, text"
    | None -> "INSERT INTO things (name) VALUES ($1) RETURNING id, name"
  in
  let params = match text with
    | Some t -> [|thing_name; t|]
    | None -> [|thing_name;|]
  in
  Db.query_and_map_single ~query:query ~params:params ~mapper:thing_mapper

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
