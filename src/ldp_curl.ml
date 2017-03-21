
module type P =
  sig
    val dbg : string -> unit Lwt.t
    val cert : string option
    val key : string option
  end

let reader_of_string str counter n =
  let len = String.length str in
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
                  Lwt.async (fun () -> Lwt_stream.closed stream >|= closefn);
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

    let rec perform conn meth =
      let%lwt () = P.dbg
         (Printf.sprintf "%s %s" (Cohttp.Code.string_of_method meth)
          (Curl.get_effectiveurl conn))
      in
      let b = Buffer.create 256 in
      Curl.set_writefunction conn (fun s -> Buffer.add_string b s; String.length s);
      let%lwt curl_code = Curl_lwt.perform conn in
      let str = Buffer.contents b in
      let code = Curl.get_responsecode conn in
      match curl_code, code with
      | Curl.CURLE_OK, 301
      | Curl.CURLE_OK, 302 ->
          begin
            let%lwt(resp, _) = read_response
              ~closefn: (fun () -> Curl.cleanup conn)
                (Cohttp.String_io.open_in str)
                ()
                meth
            in
            let url = Curl.get_redirecturl conn in
            Curl.set_url conn url ;
            perform conn meth
          end
      | _ -> Lwt.return (curl_code, str)

    let call ?body ?chunked ?headers meth iri =
      let headers =
        match headers with
          None -> Cohttp.Header.init ()
        | Some h -> h
      in
      (* set empty Expect field:
        http://www.iandennismiller.com/posts/curl-http1-1-100-continue-and-multipartform-data-post.html
      *)
      let headers = Cohttp.Header.add headers "Expect" "" in
      let headers =
        match cookies_by_iri iri with
            [] -> headers
          | cookies ->
            (*List.iter
              (fun (k, v) -> prerr_endline (Printf.sprintf "setting cookie: %s => %s" k v))
              cookies;*)
            let (k, v) = Cohttp.Cookie.Cookie_hdr.serialize cookies in
            Cohttp.Header.add headers k v
      in
      let conn = Curl.init () in
      Curl.set_header conn true ;
      Curl.set_url conn (Iri.to_uri iri) ;

      Curl.set_sslverifypeer conn true;
      Curl.set_sslverifyhost conn Curl.SSLVERIFYHOST_HOSTNAME;

      begin
        match P.cert, P.key with
          Some cert, Some key ->
            Curl.set_sslcert conn cert ;
            Curl.set_sslkey conn key
        | _ ->
            ()
      end;
      (* uncomment this not to verify host *)
            Curl.set_sslverifypeer conn false;
            Curl.set_sslverifyhost conn Curl.SSLVERIFYHOST_NONE;

      begin
        match String.uppercase_ascii (Cohttp.Code.string_of_method meth) with
        | "PUT" -> Curl.set_put conn true
        | "POST" -> Curl.set_post conn true
        | met -> Curl.set_customrequest conn met
      end;
      let%lwt () =
        match body with
          None -> Lwt.return_unit
        | Some b ->
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
            let counter = ref 0 in
            let readf = reader_of_string in
            Curl.set_readfunction conn (readf str counter);
            Lwt.return_unit
      in
      let headlines =
        List.map
          (fun (h,v) -> Printf.sprintf "%s: %s" h v)
          (Cohttp.Header.to_list headers)
      in
      Curl.set_httpheader conn headlines ;

      let%lwt (resp, body) =
        match%lwt perform conn meth with
          (Curl.CURLE_OK, str) ->
            begin
              (*prerr_endline str;*)
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

let make ?cache ?cert ~dbg =
  let (cert,privkey) =
    match cert with
      Some (cert,key) -> (Some cert, Some key)
    | None -> (None, None)
  in
  let module P =
  struct
    let dbg = dbg
    let cert = cert
    let key = privkey
  end
  in
  let%lwt cache =
    match cache with
      None -> Lwt.return (module Ldp_http.No_cache : Ldp_http.Cache)
    | Some dir -> Ldp_cache.of_dir dir
  in
  let module C = (val cache: Ldp_http.Cache) in
  let module H = Ldp_http.Cached_http (C) (Make(P)) in
  Lwt.return (module H : Ldp_http.Http)
