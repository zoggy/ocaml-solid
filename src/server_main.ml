(** Main module of SOLID server *)

let conf_options = Server_conf.add_options Ocf.group

let options =
  [ "-c", Arg.String (Ocf.from_file conf_options),
    "file load configuration from file" ;

    "--dump-options",
    Arg.Unit (fun () -> print_endline (Ocf.to_string conf_options)),
    "print current configuration" ;
  ]

let usage = Printf.sprintf "Usage: %s [options]\nwhere options are:" Sys.argv.(0)
let main () =
  match Arg.parse options (fun _ -> ()) usage with
  | exception Arg.Bad msg -> Lwt.fail_with msg
  | exception Ocf.Error e -> Lwt.fail_with (Ocf.string_of_error e)
  | () ->
     Server_log._app_lwt (fun m -> m "Starting server")

let () =
  Logs.set_reporter (Server_log.lwt_reporter ());
  try Lwt_main.run (main ())
  with Failure msg | Sys_error msg -> prerr_endline msg ; exit 1
