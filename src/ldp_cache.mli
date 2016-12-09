exception Not_a_directory of string
val of_dir : string -> (module Ldp_http.Cache) Lwt.t
