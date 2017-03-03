(** Logging of the server. *)

val src: Logs.src

val _app: 'a Logs.log
val _err: 'a Logs.log
val _warn: 'a Logs.log
val _info: 'a Logs.log
val _debug: 'a Logs.log

val _app_lwt: 'a Logs_lwt.log
val _err_lwt: 'a Logs_lwt.log
val _warn_lwt: 'a Logs_lwt.log
val _info_lwt: 'a Logs_lwt.log
val _debug_lwt: 'a Logs_lwt.log

val log_level : Logs.level option Ocf.conf_option

val lwt_reporter : unit -> Logs.reporter
