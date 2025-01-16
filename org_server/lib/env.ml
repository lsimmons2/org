type environment =
  | Dev
  | Test

type config = {
  set_db_dir : string;
}

let dev_config = {
  set_db_dir="/Users/leo/dev/org/set_db/dev/"
}

let test_config = {
  set_db_dir="/Users/leo/dev/org/set_db/test/"
}



let current_environment () =
  match Sys.getenv_opt "ORG_ENV" with
  | Some "dev" -> Dev
  | Some "development" -> Dev
  | Some "test" -> Test
  | _ -> Dev  (* Default to Dev if not specified *)

let string_of_env env =
  match env with
  | Dev -> "development"
  | Test -> "test"


let get_config () =
  let env = current_environment () in
  match env with
  | Dev -> dev_config
  | Test -> test_config
