open Lwt.Infix

open Ldp_containers

let string_of_tree tree =
  let b = Buffer.create 256 in
  let p fmt = Printf.bprintf b fmt in
  let rec iter margin t =
    p "%s%s\n" margin (Ldp_containers.node_text t);
    match t with
      CONT (_,l) ->
        List.iter (iter (margin^"  ")) l
    | _ -> ()
  in
  iter "" tree ;
  Buffer.contents b

let main =
  let%lwt (args, http) = Tls_common.parse () in
  match args with
    [] -> Lwt.return_unit
  | iri :: _ ->
      let module H = (val http : Ldp_http.Http) in
      let module C = Ldp_containers.Make(H) in
      let%lwt tree = C.containers (Iri.of_string iri) in
      Lwt_io.write Lwt_io.stdout (string_of_tree tree)


let () = Lwt_main.run main