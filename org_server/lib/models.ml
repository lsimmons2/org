
(* internal and external *)
type tag = {id:int; name:string; text:string option;} [@@deriving yojson]
type thing = {id:int; name:string; text:string option; tags: tag list;} [@@deriving yojson]
type tag_to_thing = {id:int; tag_id:int; thing_id:int;} [@@deriving yojson]
type set = {
  id:int;
  name: string;
  text: string option;
  yes_tag_ids: int list;
  no_tag_ids: int list;
} [@@deriving yojson]

(* api dtos *)
(* TODO: rename get_response *)
type 'a get_response = {success:bool; message:string; data: 'a option;} [@@deriving yojson]
type create_thing_body = {name:string; text:string option;} [@@deriving yojson]
type create_tag_body = {name:string; text:string option;} [@@deriving yojson]
type tag_thing_body = {tag_id:int; thing_id:int} [@@deriving yojson]
type create_set_body = {name:string; text:string option;} [@@deriving yojson]
type update_set_body = {
  set_id: int;
  name: string option;
  text: string option;
  yes_ids_to_add: int list option;
  yes_ids_to_remove: int list option;
  no_ids_to_add: int list option;
  no_ids_to_remove: int list option;
} [@@deriving yojson]
