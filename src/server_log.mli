(** Logging of the server. *)

val src: Logs.src

val _err: 'a Logs_lwt.log
val _warn: 'a Logs_lwt.log
val _info: 'a Logs_lwt.log
val _debug: 'a Logs_lwt.log

val log_level : Logs.level option Ocf.conf_option

