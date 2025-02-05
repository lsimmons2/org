type environment =
  | Dev
  | Test
  | Prod

type config = {
  set_db_dir : string;
  api_port: int;
}

let prod_config = {
  set_db_dir="/Users/leo/dev/org/set_db/prod/";
  api_port=7777
}

let dev_config = {
  set_db_dir="/Users/leo/dev/org/set_db/dev/";
  api_port=7776
}

let test_config = {
  set_db_dir="/Users/leo/dev/org/set_db/test/";
  api_port=7775
}



let current_environment () =
  match Sys.getenv_opt "ORG_ENV" with
  | Some "dev" -> Dev
  | Some "development" -> Dev
  | Some "prod" -> Prod
  | Some "production" -> Prod
  | Some "test" -> Test
  | _ -> Dev  (* Default to Dev if not specified *)

let string_of_env env =
  match env with
  | Prod -> "production"
  | Dev -> "development"
  | Test -> "test"


let get_config () =
  let env = current_environment () in
  match env with
  | Dev -> dev_config
  | Test -> test_config
  | Prod -> prod_config
