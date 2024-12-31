
(* internal and external *)
type tag = {id:int; name:string; text:string option;} [@@deriving yojson]
type thing = {id:int; name:string; text:string option; tags: tag list;} [@@deriving yojson]
type tag_to_thing = {id:int; tag_id:int; thing_id:int;} [@@deriving yojson]

(* api dtos *)
type 'a get_response = {success:bool; message:string; data: 'a option;} [@@deriving yojson]
(* type 'a created_response = {success:bool; message:string; created: 'a option;} [@@deriving yojson] *)
type create_thing_body = {name:string; text:string option;} [@@deriving yojson]
type create_tag_body = {name:string; text:string option;} [@@deriving yojson]
type tag_thing_body = {tag_id:int; thing_id:int} [@@deriving yojson]
