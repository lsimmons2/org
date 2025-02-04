
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
  things: thing list;
} [@@deriving yojson]

(* api dtos *)
(* TODO: rename get_response *)
type 'a get_response = {success:bool; message:string; data: 'a option;} [@@deriving yojson]
type create_thing_body = {name:string; text:string option;} [@@deriving yojson]
type create_tag_body = {name:string; text:string option;} [@@deriving yojson]
type tag_thing_body = {tag_id:int; thing_id:int} [@@deriving yojson]
type create_set_body = {name:string; text:string option;} [@@deriving yojson]

type update_set_body = {
  name: string option;
  text: string option;
  yes_ids_to_add: int list option;
  yes_ids_to_remove: int list option;
  no_ids_to_add: int list option;
  no_ids_to_remove: int list option;
} [@@deriving yojson]

type update_thing_body = {
  name: string option;
  text: string option;
} [@@deriving yojson]


type set_rest = {
  id:int;
  name: string;
  text: string option;
  yes_tags: tag list;
  no_tags: tag list;
  things: thing list;
} [@@deriving yojson]

type tag_rest = {
  id:int;
  name: string;
  text: string option;
  things: thing list;
} [@@deriving yojson]


type entity_type_rest = Thing | Tag | Set_ [@@deriving yojson]

let entity_type_rest_to_yojson = function
  | Thing -> `String "Thing"
  | Tag -> `String "Tag"
  | Set_ -> `String "Set_"

let entity_type_rest_of_yojson = function
  | `String "Thing" -> Ok Thing
  | `String "Tag" -> Ok Tag
  | `String "Set_" -> Ok Set_
  | _ -> Error "Invalid entity_type_rest"

type goto_candidate_rest = {
  entity_id:int;
  entity_name: string;
  entity_type: entity_type_rest;
  entity_text: string option;
} [@@deriving yojson]
