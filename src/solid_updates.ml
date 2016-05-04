
type updates = WebSockets.webSocket Js.t

let create iri =
  let uri = Iri.to_uri iri in
  let ws = new%js WebSockets.webSocket (Js.string uri) in
  let (waiter, wakener) = Lwt.wait () in
  let connected = ref false in
  ws##.onopen := Dom.handler
    (fun _ -> connected := true; Lwt.wakeup wakener ws; Js._true);
  ws##.onclose := Dom.handler (fun _ ->
     if not !connected
     then Lwt.wakeup_exn wakener (Failure ("Could not connect to "^uri));
     Js._true);
  waiter

let sub (websocket : updates) iri =
  let msg = Printf.sprintf "sub %s" (Iri.to_string iri) in
  ignore(websocket##send(Js.string msg))

let pub_str = "pub "
let len_pub_str = String.length pub_str

let pub_handler f _ event =
  let msg = Js.to_string event##.data in
  let len = String.length msg in
  if len > len_pub_str &&
    String.sub msg 0 len_pub_str = pub_str
  then
    (
     let s = String.sub msg
       len_pub_str (len - len_pub_str)
     in
     f (Iri.of_string s)
    );
  Js._true

let on_pub websocket f =
  websocket##.onmessage := Dom.full_handler (pub_handler f)

    