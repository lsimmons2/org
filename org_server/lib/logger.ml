[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]



open Logs

type log_level = Debug | Info | Warn | Error

let to_logs_level = function
  | Debug -> Logs.Debug
  | Info -> Logs.Info
  | Warn -> Logs.Warning
  | Error -> Logs.Error

let set_level level =
  Logs.set_level (Some (to_logs_level level))

let () =
  let log_level = match Sys.getenv_opt "LOG_LEVEL" with
    | Some "debug" -> Logs.Debug
    | Some "info" -> Logs.Info
    | Some "warning" -> Logs.Warning
    | Some "error" -> Logs.Error
    | _ -> Logs.Info
  in
  Printf.printf "\nSetting up logging with level %s\n%!" (Logs.level_to_string (Some log_level));
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some log_level)

let log_message level fmt =
  let logs_level = to_logs_level level in
  Format.kasprintf
    (fun formatted_str ->
       Logs.msg logs_level (fun m -> m "%s" formatted_str))
    fmt

let log_message_lwt level fmt =
  let logs_level = to_logs_level level in
  Format.kasprintf
    (fun formatted_str ->
       Logs.msg logs_level (fun m -> m "%s" formatted_str);
       Lwt.return_unit)
    fmt


(* Exposed logging functions with formatting support *)
let info fmt = log_message Info fmt
let debug fmt = log_message Debug fmt
let warn fmt = log_message Warn fmt
let error fmt = log_message Error fmt

let info_lwt fmt = log_message_lwt Info fmt
let debug_lwt fmt = log_message_lwt Debug fmt
let warn_lwt fmt = log_message_lwt Warn fmt
let error_lwt fmt = log_message_lwt Error fmt
