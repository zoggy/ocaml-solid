(** *)

let src = Logs.Src.create ~doc:"log of the ocaml-solid library" "solid"


let __err f = Logs_lwt.err ~src f
let __warn f = Logs_lwt.warn ~src f
let __info f = Logs_lwt.info ~src f
let __debug f = Logs_lwt.debug ~src f


