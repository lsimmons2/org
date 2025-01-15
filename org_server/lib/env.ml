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
  | Some "test" -> Test
  | _ -> Dev  (* Default to Dev if not specified *)

let string_of_env env =
  match env with
  | Dev -> "development"
  | Test -> "test"


let get_config_for_env env =
  match env with
  | Dev -> dev_config
  | Test -> test_config

let get_config =
  let env = current_environment () in
  get_config_for_env env

(* let () = *)
(*   let env = current_environment () in *)
(*   let config = get_config env in *)
(*   Printf.printf "Running in environment: %s\n" (match env with *)
(*       | Dev -> "Development" *)
(*       | Test -> "Test"); *)
(*   Printf.printf "Database URL: %s\n" config.; *)
(*   Printf.printf "API Key: %s\n" config.api_key; *)
(*   Printf.printf "Log Level: %s\n" config.log_level; *)
