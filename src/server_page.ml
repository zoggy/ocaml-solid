(** *)

let mime_xhtml = "application/xhtml+xml"
let mime_xhtml_charset = mime_xhtml^";charset=utf-8"

let page title contents =
  let module Xh = Xtmpl_xhtml in
  let module X = Xtmpl_rewrite in
  let xml =
    Xh.html
      ~atts: (X.atts_one ("","xmlns") [X.cdata "http://www.w3.org/1999/xhtml"])
      [
        Xh.header
          [
            Xh.title [X.cdata title] ;
            Xh.meta ~atts:(X.atts_of_list
             [ ("","http-equiv"), [X.cdata "Content-Type"] ;
               ("","content"), [X.cdata mime_xhtml_charset] ;
             ]) [] ;
          ];
        Xh.body contents ;
      ]
  in
  X.to_string [xml]