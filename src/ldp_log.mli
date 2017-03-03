(** Logging of the library. *)

val src: Logs.src

val __err: 'a Logs_lwt.log
val __warn: 'a Logs_lwt.log
val __info: 'a Logs_lwt.log
val __debug: 'a Logs_lwt.log

val level_wrapper: Logs.level option Ocf.wrapper
val log_level : Logs.level option Ocf.conf_option

