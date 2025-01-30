type log_level = Debug | Info | Warn | Error

val set_level : log_level -> unit

val info : ('a, Format.formatter, unit, unit) format4 -> 'a
val debug : ('a, Format.formatter, unit, unit) format4 -> 'a
val warn : ('a, Format.formatter, unit, unit) format4 -> 'a
val error : ('a, Format.formatter, unit, unit) format4 -> 'a

val debug_lwt : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
val info_lwt : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
val warn_lwt : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
val error_lwt : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
