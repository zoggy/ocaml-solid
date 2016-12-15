
module type P =
  sig
    val dbg : string -> unit Lwt.t
    val cert : string
    val key : string
  end

let reader_of_string str =
  let counter = ref 0 in
  let len = String.length str in
  let f n =
    if !counter >= len then
      (
      ""
      )
    else
      if !counter + n < len then
        (
         let s = String.sub str !counter n in
         counter := !counter + n;
         s
        )
      else
        (
         let s = String.sub str !counter (len - !counter) in
         counter := !counter + n ;
         s
        )
  in
  f

module Make (P:P) : Ldp_http.Requests =
  struct
    let () = Curl.global_init Curl.CURLINIT_GLOBALALL
    (*let () = Curl_lwt.set_debug true*)
    let dbg = P.dbg

    include Ldp_cookies.Make ()

    module IO = String_io_lwt
    module Response = Cohttp.Response.Make (IO)
    module Body = Cohttp_lwt_body

    open Lwt.Infix

    (* from cohttp_lwt *)
    let read_response ~closefn ic oc meth =
      Response.read ic >>= begin function
        | `Invalid reason ->
            Lwt.fail (Failure ("Failed to read response: " ^ reason))
        | `Eof -> Lwt.fail (Failure "Client connection was closed")
        | `Ok res -> begin
              let has_body = match meth with
                | `HEAD | `DELETE -> `No
                | _ -> Response.has_body res
              in
              match has_body with
              | `Yes | `Unknown ->
                  let reader = Response.make_body_reader res ic in
                  let stream = Body.create_stream Response.read_body_chunk reader in
                  Lwt_stream.on_terminate stream closefn;
                  let gcfn st = closefn () in
                  Gc.finalise gcfn stream;
                  let body = Body.of_stream stream in
                  Lwt.return (res, body)
              | `No -> closefn (); Lwt.return (res, `Empty)
            end
      end
        |> fun t ->
        Lwt.on_cancel t closefn;
      Lwt.on_failure t (fun _exn -> closefn ());
      t

    let perform h =
      let b = Buffer.create 256 in
      Curl.set_writefunction h (fun s -> Buffer.add_string b s; String.length s);
      Lwt.bind (Curl_lwt.perform h) (fun code -> Lwt.return (code, Buffer.contents b))

    let call ?body ?chunked ?headers meth iri =
      let headers =
        match headers, cookies_by_iri iri with
            None, [] -> None
          | _, cookies ->
            let h =
              match headers with
                None -> Cohttp.Header.init ()
              | Some h -> h
            in
            (*List.iter
              (fun (k, v) -> prerr_endline (Printf.sprintf "setting cookie: %s => %s" k v))
              cookies;*)
            let (k, v) = Cohttp.Cookie.Cookie_hdr.serialize cookies in
            Some (Cohttp.Header.add h k v)
      in
      let conn = Curl.init () in
      Curl.set_url conn (Iri.to_uri iri) ;
      Curl.set_followlocation conn true;
      Curl.set_sslverifypeer conn true;
      Curl.set_sslverifyhost conn Curl.SSLVERIFYHOST_HOSTNAME;
      Curl.set_sslcert conn P.cert ;
      Curl.set_sslkey conn P.key ;
      begin
        match String.uppercase_ascii (Cohttp.Code.string_of_method meth) with
        | "PUT" -> Curl.set_put conn true
        | "POST" -> Curl.set_post conn true
        | met -> Curl.set_customrequest conn met
      end;
      let () =
        match headers with
          None -> ()
        | Some h ->
            Curl.set_header conn true ;
            Curl.set_httpheader conn (Cohttp.Header.to_lines h)
      in
      let%lwt () =
        match body with
          None -> Lwt.return_unit
        | Some b ->
            (*let%lwt () = Lwt_io.(write_line stderr "with body") in*)
            let%lwt str = Body.to_string b in
            (*let readfunction = reader_of_string str in
            let%lwt () =
              let b = Buffer.create 256 in
              let rec iter () =
                match readfunction 40 with
                   "" -> Lwt_io.(write_line stderr (Buffer.contents b) )
                 | s -> assert (String.length s <= 40); Buffer.add_string b s ; iter ()
              in
              iter ()
            in*)
            Curl.set_upload conn true;
            let len = String.length str in
            Curl.set_infilesize conn len ;
            Curl.set_readfunction conn (reader_of_string str);
            Lwt.return_unit
      in
      let%lwt (resp, body) =
        match%lwt perform conn with
          (Curl.CURLE_OK, str) ->
            begin
              let code = Curl.get_responsecode conn in
              match code / 100 with
                2 ->
                  read_response
                    ~closefn: (fun () -> Curl.cleanup conn)
                    (Cohttp.String_io.open_in str)
                    ()
                    meth
              | _ ->
                  Curl.cleanup conn ;
                  Ldp_types.(fail (Request_error (iri, string_of_int code)))
            end
        | (code, _) ->
            Curl.cleanup conn ;
            Ldp_types.(fail (Request_error (iri, Curl.strerror code)))
      in
      let () =
        let cookies = Cohttp.Cookie.Set_cookie_hdr.extract resp.Cohttp.Response.headers in
        match cookies with
        | [] -> ()
        | _ ->
            remove_cookies () ;
            List.iter (add_cookie iri) cookies ;
      in
      Lwt.return (resp, body)
  end