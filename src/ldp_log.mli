(** Logging of the library. *)

val src: Logs.src

val __err: 'a Logs.log
val __warn: 'a Logs.log
val __info: 'a Logs.log
val __debug: 'a Logs.log

val __err_lwt: 'a Logs_lwt.log
val __warn_lwt: 'a Logs_lwt.log
val __info_lwt: 'a Logs_lwt.log
val __debug_lwt: 'a Logs_lwt.log

val level_wrapper: Logs.level option Ocf.wrapper
val log_level : Logs.level option Ocf.conf_option

