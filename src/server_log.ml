(** *)

let src = Logs.default

let _err f = Logs_lwt.err ~src f
let _warn f = Logs_lwt.warn ~src f
let _info f = Logs_lwt.info ~src f
let _debug f = Logs_lwt.debug ~src f

let log_level = Ocf.option
  ~cb: Logs.set_level
  Ldp_log.level_wrapper None