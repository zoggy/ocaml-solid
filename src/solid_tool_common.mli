type profile = {
  id : string;
  privkey : string;
  cert : string;
  certificates : string option;
  cache : string option;
}
val default_profile : profile
val profile_wrapper : profile Ocf.Wrapper.t
val usage : string
val ldp_http_curl : profile -> (module Ldp_http.Http) Lwt.t
val ldp_http_tls : profile -> (module Ldp_http.Http) Lwt.t
val profiles : profile list Ocf.conf_option
val map_filename : ?dir:string -> string -> string
val find_profile : string -> profile
val parse :
  ?options:(Arg.key * Arg.spec * Arg.doc) list ->
  ?usage:Arg.usage_msg ->
  unit -> (string list * (module Ldp_http.Http)) Lwt.t
val print_alert : string -> Tls.Packet.alert_type -> unit Lwt.t
val print_fail : string -> Tls.Engine.failure -> unit Lwt.t
val main :
  ?options:(Arg.key * Arg.spec * Arg.doc) list ->
  ?usage:Arg.usage_msg ->
  (string list -> (module Ldp_http.Http) -> unit Lwt.t) -> unit
