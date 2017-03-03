(** *)

let src = Logs.default

let _app f = Logs.app ~src f
let _err f = Logs.err ~src f
let _warn f = Logs.warn ~src f
let _info f = Logs.info ~src f
let _debug f = Logs.debug ~src f

let _app_lwt f = Logs_lwt.app ~src f
let _err_lwt f = Logs_lwt.err ~src f
let _warn_lwt f = Logs_lwt.warn ~src f
let _info_lwt f = Logs_lwt.info ~src f
let _debug_lwt f = Logs_lwt.debug ~src f

let log_level = Ocf.option
  ~cb: Logs.set_level
  Ldp_log.level_wrapper None

let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () = match level with
      | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
      | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
  in
  { Logs.report = report }
