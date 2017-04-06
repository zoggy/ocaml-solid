(** *)

open Rdf_graph
open Rdf_term

type path_elt = [`S of string | `I of Iri.t]
type path = path_elt list

module P = struct
  type t = path_elt
  let compare (e1:t) (e2:t) =
    match e1, e2 with
        `S s1, `S s2 -> Pervasives.compare s1 s2
      | `S _, `I _ -> 1
      | `I _, `S _ -> -1
      | `I i1, `I i2 -> Iri.compare i1 i2
  end


module PMap = Map.Make(P)

let string_of_path l =
  let f = function
    `S s -> s
  | `I i -> Iri.to_string i
  in
  String.concat "." (List.map f l)

type error =
| Invalid_value of Rdf_term.term
| Invalid_path of path
| Path_conflict of path
| Error_at_path of path * error
| Exn_at_path of path * exn

exception Error of error

let rec string_of_error = function
| Invalid_value term ->
    Printf.sprintf "Invalid value %s" (Rdf_term.string_of_term term)
| Invalid_path p -> Printf.sprintf "Invalid path %S" (string_of_path p)
| Path_conflict p -> Printf.sprintf "Path conflict on %S" (string_of_path p)
| Error_at_path (p, e) ->
    Printf.sprintf "%S: %s" (string_of_path p) (string_of_error e)
| Exn_at_path (p, e) ->
    Printf.sprintf "%S: %s" (string_of_path p) (Printexc.to_string e)

let error e = raise (Error e)
let invalid_value t = error (Invalid_value t)
let invalid_path p = error (Invalid_path p)
let path_conflict p = error (Path_conflict p)
let error_at_path p e = error (Error_at_path (p, e))
let exn_at_path p e = error (Exn_at_path (p, e))

module Wrapper =
  struct
    type 'a t = {
        to_term : ?with_doc: bool -> Rdf_graph.graph -> 'a -> Rdf_term.term ;
        from_term : ?def: 'a -> Rdf_graph.graph -> Rdf_term.term ->  'a ;
      }

    let make to_term from_term = { to_term ; from_term }

    let int =
      let to_term ?with_doc g n = Rdf_term.term_of_int n in
      let from_term ?def _ t =
        match t with
          Literal lit ->
            (try int_of_string lit.lit_value
             with _ -> invalid_value t)
        | _ -> invalid_value t
      in
      make to_term from_term

    let float =
      let to_term ?with_doc g n = Rdf_term.term_of_double n in
      let from_term ?def _ t =
        match t with
          Literal lit ->
            (try float_of_string lit.lit_value
             with _ -> invalid_value t)
        | _ -> invalid_value t
      in
      make to_term from_term

    let bool =
      let to_term ?with_doc g b = Rdf_term.term_of_bool b in
      let from_term ?def _ t =
        match t with
          Literal lit ->
            (try bool_of_literal lit
             with _ -> invalid_value t)
        | _ -> invalid_value t
      in
      make to_term from_term

    let string_ ?typ to_s of_s =
      let to_term ?with_doc g x = Rdf_term.term_of_literal_string ?typ (to_s x) in
      let from_term ?def _ t =
        match t with
          Literal lit ->
            (try of_s lit.lit_value
             with _ -> invalid_value t)
        | _ -> invalid_value t
      in
      make to_term from_term

    let string = string_ (fun x -> x) (fun x -> x)
    let typed_string typ = string_ ~typ  (fun x -> x) (fun x -> x)

    let add_list g l =
      let rec add term tail =
        let sub = Rdf_term.blank_ (g.new_blank_id ()) in
        g.add_triple ~sub ~pred:Rdf_rdf.rest ~obj:tail ;
        g.add_triple ~sub ~pred:Rdf_rdf.first ~obj:term ;
        sub
      in
      List.fold_right add l (Iri Rdf_rdf.nil)

    let read_list g head =
      let rec iter acc sub =
        let acc =
          match g.objects_of ~sub ~pred: Rdf_rdf.first with
            [] -> acc
          | term :: _ -> term :: acc
        in
        match g.objects_of ~sub ~pred: Rdf_rdf.rest with
          [] -> List.rev acc
        | (Iri h) :: _ when Iri.equal h Rdf_rdf.nil -> List.rev acc
        | h :: _ -> iter acc h
      in
      iter [] head

    let list ?typ w =
      let to_term ?with_doc g l =
        let terms = List.map (w.to_term ?with_doc g) l in
        let head = add_list g terms in
        (
         match typ with
         | None -> ()
         | Some typ ->
             match head with
               Iri head when Iri.equal head Rdf_rdf.nil -> ()
             | _ -> g.add_triple ~sub:head ~pred: Rdf_rdf.type_ ~obj:(Iri typ)
        );
        head
      in
      let from_term ?def g t =
        let terms = read_list g t in
        List.map (w.from_term ?def:None g) terms
      in
      make to_term from_term

    let option w =
      let to_term ?with_doc g = function
        None -> Iri Rdf_rdf.nil
      | Some v -> w.to_term ?with_doc g v
      in
      let from_term ?def g t =
        match t with
          Iri i when Iri.equal i Rdf_rdf.nil -> None
        | _ -> Some (w.from_term g t)
      in
      make to_term from_term

    let pair w1 w2 =
      let to_term ?with_doc g (v1, v2) =
       let terms = [
            w1.to_term ?with_doc g v1 ;
            w2.to_term ?with_doc g v2 ;
          ]
        in
        add_list g terms
      in
      let from_term ?def g t =
        match read_list g t with
          [t1 ; t2] -> (w1.from_term g t1, w2.from_term g t2)
        | _ -> invalid_value t
      in
      make to_term from_term

    let triple w1 w2 w3 =
      let to_term ?with_doc g (v1, v2, v3) =
       let terms = [
            w1.to_term ?with_doc g v1 ;
            w2.to_term ?with_doc g v2 ;
            w3.to_term ?with_doc g v3 ;
          ]
        in
        add_list g terms
      in
      let from_term ?def g t =
        match read_list g t with
          [t1 ; t2 ; t3] -> (w1.from_term g t1, w2.from_term g t2, w3.from_term g t3)
        | _ -> invalid_value t
      in
      make to_term from_term
  end

type 'a wrapper = 'a Wrapper.t
type conf_option_ =
  { wrapper : 'a. 'a wrapper ;
    mutable value : 'a. 'a ;
    doc : string option ;
    cb : 'a. ('a -> unit) option ;
  }

type 'a conf_option = conf_option_

let get o = o.value
let set (o : 'a conf_option) (v : 'a) =
  o.value <- Obj.magic v;
  match o.cb with
  | None -> ()
  | Some f -> f v

let option : ?doc: string -> ?cb: ('a -> unit) ->
  'a wrapper -> 'a -> 'a conf_option =
  fun ?doc ?cb wrapper value ->
    { wrapper = Obj.magic wrapper ;
      value = Obj.magic value ;
      doc ;
      cb = Obj.magic cb ;
    }

let int ?doc ?cb n = option ?doc ?cb Wrapper.int n
let float ?doc ?cb x = option ?doc ?cb Wrapper.float x
let bool ?doc ?cb x = option ?doc ?cb Wrapper.bool x
let string ?doc ?cb s = option ?doc ?cb Wrapper.string s
let list ?doc ?cb w l = option ?doc ?cb (Wrapper.list w) l
let option_ ?doc ?cb w l = option ?doc ?cb (Wrapper.option w) l
let pair ?doc ?cb w1 w2 x = option ?doc ?cb (Wrapper.pair w1 w2) x
let triple ?doc ?cb w1 w2 w3 x = option ?doc ?cb (Wrapper.triple w1 w2 w3) x

type node =
  | Option of conf_option_
  | Group of node PMap.t

and 'a group = node
let group = Group PMap.empty

let rec add ?(acc_path=[]) group path node =
  match path with
    [] -> invalid_path []
  | [h] ->
      begin
        match PMap.find h group with
        | exception Not_found ->
            PMap.add h node group
        | _ ->
            path_conflict (List.rev (h::acc_path))
      end
  | h :: q ->
      match PMap.find h group with
      | exception Not_found ->
          let map = add
            ~acc_path: (h::acc_path) PMap.empty q node
          in
          PMap.add h (Group map) group
      | Option _ ->
          path_conflict (List.rev (h::acc_path))
      | Group _ when q = [] ->
          path_conflict (List.rev (h::acc_path))
      | Group map ->
          let map = add
            ~acc_path: (h::acc_path) map q node
          in
          PMap.add h (Group map) group

let add_group group path g =
  match group with
    Option _ -> assert false
  | Group map -> Group (add ?acc_path: None map path g)

let add group path option =
  match group with
  | Option _ -> assert false
  | Group map -> Group (add ?acc_path: None map path (Option option))

let as_group o = Option o

let from_term_option path option g term =
  try
    let v = option.wrapper.Wrapper.from_term
      ~def: option.value g term
    in
    set option v
  with
    Error e -> error_at_path path e
  | e -> exn_at_path path e


let assocs g sub =
  let ds = Rdf_ds.simple_dataset g in
  let q = [%sparql
    "PREFIX rdf: %{term}
     SELECT ?name ?term
     WHERE { %{term} rdf:value _:p .
             _:p rdf:ID ?name .
             _:p rdf:value ?term .}"
       (Iri Rdf_rdf.rdf) sub]
  in
  let map =
    try
      let solutions = Rdf_sparql.select ~base:(g.name()) ds q in
      List.fold_left
        (fun acc sol ->
           let k = `S (Rdf_sparql.get_string sol "name") in
           let t = Rdf_sparql.get_term sol "term" in
           PMap.add k t acc)
        PMap.empty
        solutions
    with
      e ->
        Ldp_log.__debug (fun f -> f "assocs: %s" (Printexc.to_string e));
        PMap.empty
  in
  let q = [%sparql
    "PREFIX rdf: %{term}
     SELECT ?pred ?term
     WHERE { %{term} ?pred ?term .
             FILTER (?pred != rdf:value)
     }"
       (Iri Rdf_rdf.rdf) sub]
  in
  let map =
    try
      let base = g.name() in
      let solutions = Rdf_sparql.select ~base ds q in
      List.fold_left
        (fun acc sol ->
           let k = `I (Rdf_sparql.get_iri sol base "pred") in
           let t = Rdf_sparql.get_term sol "term" in
           PMap.add k t acc)
        map
        solutions
    with
      e ->
        Ldp_log.__debug (fun f -> f "assocs: %s" (Printexc.to_string e));
        map
  in
  map

let rec from_term_group =
  let f path (assocs:Rdf_term.term PMap.t) g str node =
    match PMap.find str assocs with
    | exception Not_found -> ()
    | term ->
        match node with
          Option o -> from_term_option (List.rev (str :: path)) o g term
        | Group map ->
            from_term_group ~path: (str :: path) map g term
  in
  fun ?(path=[]) map g term ->
    let assocs = assocs g term in
    PMap.iter (f path assocs g) map

let from_term = function
  Option o -> from_term_option [] o
| Group g -> from_term_group ?path: None g

let from_graph map ?root g =
  let term = match root with
      None -> Iri (g.name ())
    | Some t -> t
  in
  from_term map g term

let add_term_option ?with_doc ?(pred=Rdf_rdf.value) option prop_node g =
  let t = option.wrapper.Wrapper.to_term ?with_doc g option.value in
  g.add_triple ~sub:prop_node ~pred ~obj:t;
  match with_doc, option.doc with
  | Some true, Some str when Iri.equal pred Rdf_rdf.value ->
      g.add_triple ~sub:prop_node
        ~pred:Rdf_rdf.description
        ~obj:(Rdf_term.term_of_literal_string str)
  | _, _ -> ()

let rec add_term_group ?with_doc ?(pred=Rdf_rdf.value) map ?(first=false) root g =
  let prop_node =
    if first then
      root
    else
      (
       let b = blank_ (g.new_blank_id()) in
       g.add_triple ~sub:root ~pred ~obj:b;
       b
      )
  in
  let f name node =
    let (group_node, pred) =
      match name with
      | `S name ->
          let b = blank_ (g.new_blank_id()) in
          g.add_triple ~sub:prop_node ~pred:Rdf_rdf.value ~obj:b;
          g.add_triple ~sub:b ~pred:Rdf_rdf.id
            ~obj:(Rdf_term.term_of_literal_string name);
          (b, Rdf_rdf.value)
      | `I iri ->
          (prop_node, iri)
    in
    match node with
    | Group map ->
        add_term_group ?with_doc ~pred map group_node g
    | Option o ->
        add_term_option ?with_doc ~pred o group_node g
  in
  PMap.iter f map

let to_graph_ ?(with_doc=true) = function
| Option o -> add_term_option ~with_doc o
| Group g -> add_term_group ~with_doc ~first: true g

let to_graph ?with_doc map base =
  let g = open_graph base in
  to_graph_ ?with_doc map (Iri base) g;
  g
