(** *)

let src = Logs.Src.create ~doc:"log of the ocaml-solid library" "solid"

let __err f = Logs_lwt.err ~src f
let __warn f = Logs_lwt.warn ~src f
let __info f = Logs_lwt.info ~src f
let __debug f = Logs_lwt.debug ~src f

open Logs

let level_wrapper =
  let to_json ?with_doc = function
    App -> `String "app"
  | Error -> `String "error"
  | Warning -> `String "warning"
  | Info -> `String "info"
  | Debug -> `String "debug"
  in
  let from_json ?(def=App) = function
  `String s ->
      begin
        match s with
          "app" | "0" -> App
        | "error" | "1" -> Error
        | "warning" | "2" -> Warning
        | "info" | "3" -> Info
        | "Debug" | "4" -> Debug
        | _ -> def
      end
  | _ -> def
  in
  let w = Ocf.Wrapper.make to_json from_json in
  Ocf.Wrapper.option w

let log_level = Ocf.option
  ~cb: (Logs.Src.set_level src)
  level_wrapper None
