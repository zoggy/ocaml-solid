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

let f args http =
  match args with
    [] -> Lwt.return_unit
  | iris ->
      let module H = (val http : Ldp_http.Http) in
      let module C = Ldp_containers.Make(H) in
      let%lwt trees = Lwt_list.map_p
        (fun iri -> C.containers (Iri.of_string iri)) iris
      in
      Lwt_list.iter_s
        (fun t -> Lwt_io.write Lwt_io.stdout (string_of_tree t))
        trees

let () = Solid_common.main f