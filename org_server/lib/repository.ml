[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]


open Lwt.Infix

let get_things () : Models.thing list = 
  let conn: Postgresql.connection = Db.db_connect `Test in
  (* No params needed for this query, so we pass an empty array *)
  let result = Db.query_db conn "SELECT * FROM things" ~params:[||] () in
  conn#finish;
  let rows = Array.to_list (result#get_all) in
  (List.map
     (fun row ->
        let rv: Models.thing =
          { Models.id = int_of_string row.(0);
            Models.name = row.(1);
            Models.text = if row.(2) = "" then None else Some row.(2) } in
        rv)
     rows)


let get_tags () : Models.tag list =
  let conn: Postgresql.connection = Db.db_connect `Test in
  let result = Db.query_db conn "SELECT * FROM tags" ~params:[||] () in
  conn#finish;
  let rows = Array.to_list (result#get_all) in
  (List.map
     (fun row ->
        {
          Models.id = int_of_string row.(0);
          name = row.(1);
          text = if row.(2) = "" then None else Some row.(2)
        })
     rows)

let tag_mapper (result : Postgresql.result) (row : int) : Models.tag =
  {
    id = int_of_string (result#getvalue row 0);  (* Assuming "id" is in column 0 *)
    name = result#getvalue row 1;              (* Assuming "name" is in column 1 *)
    text = let text_val = result#getvalue row 2 in
      if text_val = "" then None else Some text_val;  (* Assuming "text" is in column 2 *)
  }

let create_tag ~name ~text =
  let conn: Postgresql.connection = Db.db_connect `Test in
  let query = "INSERT INTO tags (name, text) VALUES ($1, $2) RETURNING id, name, text" in
  Db.query_and_return_created conn query ~params:[|name; text|] ~mapper:tag_mapper
