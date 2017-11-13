module OV = Ocaml_version
module C = Dockerfile_cmd
module D = Dockerfile_distro
open Soup
open Astring
open Bos
open Rresult
open R.Infix

let idx () =
  let css = "body { padding: 0.5rem; }" in
  Fmt.strf
    "\n<!doctype html>\n<html lang=\"en\">\n  <head>\n    <title class=\"template\"></title>\n    <meta charset=\"utf-8\">\n    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\">\n    <link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css\" integrity=\"sha384-PsH8R72JQ3SOdhVi3uxftmaW6Vc51MKb0q5P2rRUpPvrszuE4W1povHYgTpBfshb\" crossorigin=\"anonymous\">\n  <style>%s</style></head>\n  <body>\n    <main role=\"main\" class=\"container\">\n    <div class=\"col-9\">\n    <div class=\"row\">\n   <div class=\"intro\"></div> <div class=\"template\"></div>\n    </div>\n    </div>\n    </main>\n    <script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/js/bootstrap.min.js\" integrity=\"sha384-alpBpkh1PFOepccYVYDB4do5UnbKysX5WZXm3XxPqe5iKTfUKjNkCk9SaVuEZflJ\" crossorigin=\"anonymous\"></script>\n  </body>\n</html>\n" css |> parse

let href href inner_text =
  create_element ~attributes:[("href", href)] ~inner_text "a"

let days_since date =
  let span =
    match Ptime.Span.of_float_s (Unix.gettimeofday () -. date) with
    | None -> failwith "invalid date"
    | Some d -> d in
  Fmt.strf "%a" Ptime.Span.pp span

let generate_index_by_version index =
  let idx = idx () in
  let content =
    let ul = create_element ~classes:["list-group"] "div" in
    let _latest =
      let (_, date, subj) = List.hd index.Obi.revs in
      let target = "latest/" in
      let li = create_element ~attributes:["href",target] ~classes:["list-group-item";"list-group-item-action";"flex-column";"align-items-start";"active"] "a" in
      append_child ul li;
      let li2 = create_element ~classes:["d-flex";"w-100";"justify-content-between"] "div" in
      append_child li li2;
      let h5 = create_element ~class_:"mb-1" ~inner_text:"Latest" "h5" in
      append_child li2 h5;
      let tr = create_element ~class_:"text-muted" ~inner_text:(days_since date) "small" in
      append_child li2 tr;
      let s = create_element ~class_:"mb-1" ~inner_text:subj "p" in
      append_child li s;
    in  
    List.iter (fun (rev, date, subj) ->
      let target = Fmt.strf "%s/index.html" (String.with_range ~len:8 rev) in
      let li = create_element ~attributes:["href",target] ~classes:["list-group-item";"list-group-item-action";"flex-column";"align-items-start"] "a" in
      append_child ul li;
      let li2 = create_element ~classes:["d-flex";"w-100";"justify-content-between"] "div" in
      append_child li li2;
      let h5 = create_element ~class_:"mb-1" ~inner_text:(String.with_range ~len:8 rev) "h5" in
      append_child li2 h5;
      let tr = create_element ~class_:"text-muted" ~inner_text:(days_since date) "small" in
      append_child li2 tr;
      let s = create_element ~class_:"mb-1" ~inner_text:subj "p" in
      append_child li s;
    ) index.Obi.revs;
    ul
  in
  let title ="Bulk build results sorted by OCaml compiler version" in
  replace (idx $ "title.template") (create_element ~inner_text:title "title") ;
  replace (idx $ "div.template") content ;
  let intro =
    let nav =
      let e =
        create_element
          ~attributes:[("aria-label", "breadcrumb"); ("role", "navigation")]
          "nav"
      in
      let ol = create_element ~class_:"breadcrumb" "ol" in
      append_child e ol ;
      append_child ol
        (create_element ~classes:["breadcrumb-item"; "active"] "li" |> fun e -> append_child e (href "../.." "Bulk Builds"); e);
      append_child ol
        (create_element ~classes:["breadcrumb-item"; "active"] ~inner_text:"By Compiler Version" "li");
      e
    in
    let e = Soup.parse (Fmt.strf "<p>Snapshots of the opam <a href=\"https://github.com/ocaml/opam-repository\">package repository</a> are regularly built against recent versions of the OCaml compiler. This page lists the most recent builds, sorted by the git revision of the repository.</p>") in
    append_child nav e ;
    nav
  in
  replace (idx $ "div.intro") intro ;
  Ok idx

let generate_by_version_for_rev ~rev ~distro ~arch logs_uri res =
  let idx = idx () in
  (* Get all OCaml versions in this result set *)
  let ovs =
    let h = Hashtbl.create 10 in
    List.iter (fun pkg ->
      List.iter (fun (_,res) ->
        List.iter (fun (ov,_) -> Hashtbl.replace h ov ()) res
      ) pkg.Obi.versions
    ) res;
    Hashtbl.fold (fun k () a -> k::a) h [] |> List.sort OV.compare in
  let table =
    create_element
      ~classes:["table"; "table-sm"; "table-responsive"]
      "table"
  in
  let _ =
    let thead = create_element ~class_:"thead-dark" "thead" in
    let th = create_element "tr" in
    append_child th
      (create_element ~classes:["col-3";"text-right";"align-top"] ~inner_text:"Package" "th") ;
    append_child th
      (create_element ~classes:["col-3";"text-left";"align-top"] ~inner_text:"Version" "th") ;
    List.iter
      (fun ov ->
        let e = match OV.extra ov with
        | None -> create_element ~classes:["align-top"] ~inner_text:(OV.to_string ov) "th"
        | Some extra ->
            let inner_text = match extra with "default-unsafe-string" -> "unsafe-string" | e -> e in
            let e = create_element ~inner_text:(OV.(with_variant ov None |> to_string)) "th" in
            append_child e (create_element ~classes:["badge"] ~inner_text "span");
            e
        in
        append_child th e)
      ovs ;
    append_child thead th;
    append_child table thead
  in
  let tbody = create_element "tbody" in
  append_child table tbody ;
  (* Combine individual versions of packages *)
  let pkg_names = List.fold_left (fun a b -> b.Obi.name :: a) [] res |> List.sort String.compare in
  List.iter
    (fun pkg_name ->
      let pkg = List.find (fun pkg -> pkg.Obi.name = pkg_name) res in
      let vs = List.fold_left (fun a (ver,_) -> ver::a) [] pkg.versions |> List.sort String.compare in
      let row = create_element "tr" in
      append_child tbody row ;
      let th =
        create_element ~class_:"text-right"
          ~attributes:[("rowspan", string_of_int (List.length vs + 1))]
          ~inner_text:pkg_name "th"
      in
      append_child row th ;
      List.iter
        (fun pkg_version ->
          let r = List.assoc pkg_version pkg.versions in
          let vrow = create_element "tr" in
          append_child vrow (create_element ~inner_text:pkg_version "td") ;
          append_child row vrow ;
          List.iter
            (fun ov ->
              match List.assoc ov r with
              | res when res.status = `Exited 0 -> 
                  append_child vrow
                    (create_element ~classes:["table-success"; "text-center"]
                       ~inner_text:"🐫" "td")
              | res ->
                  append_child vrow
                    (create_element ~classes:["table-danger"; "text-center"] "td" |> fun e -> append_child e (href (logs_uri ^ res.log_hash ^ ".txt") "🙁"); e)
              | exception Not_found ->
                  append_child vrow
                    (create_element ~classes:["table-secondary"; "text-center"]
                       ~inner_text:"" "td"))
            ovs)
        vs)
    pkg_names ;
  let short_hash = String.with_range ~len:8 rev in
  let title =
    Fmt.strf "Bulk build results for opam-repository revision %s" short_hash
  in
  replace (idx $ "title.template") (create_element ~inner_text:title "title") ;
  replace (idx $ "div.template") table ;
  let intro =
    let nav =
      let e =
        create_element
          ~attributes:[("aria-label", "breadcrumb"); ("role", "navigation")]
          "nav"
      in
      let ol = create_element ~class_:"breadcrumb" "ol" in
      append_child e ol ;
      append_child ol
        (create_element ~classes:["breadcrumb-item"; "active"] "li" |> fun e -> append_child e (href "../.." "Bulk Builds"); e);
      append_child ol
        (create_element ~classes:["breadcrumb-item"; "active"] "li" |> fun e -> append_child e (href "../" "By Compiler Version"); e);
      append_child ol
        (create_element ~classes:["breadcrumb-item"; "active"]
           ~attributes:[("aria-current", "page")] ~inner_text:short_hash "li") ;
      e
    in
    let e = Soup.parse (Fmt.strf "<p>These are the <a href=\"https://opam.ocaml.org/\">opam</a> bulk build results for the packages at <a href=\"https://github.com/ocaml/opam-repository\">opam-repository</a> revision <a href=\"https://github.com/ocaml/opam-repository/commit/%s\">%s</a>, built using %s on an %s architecture.</p>" short_hash short_hash (D.human_readable_string_of_distro distro) (OV.string_of_arch arch)) in
    append_child nav e ;
    nav
  in
  replace (idx $ "div.intro") intro ;
  Ok idx

let write_html dir html =
  Logs.info (fun l -> l "Generating: %a" Fpath.pp dir);
  OS.File.write dir (Soup.to_string html)

let generate logs_uri meta_dir logs_dir html_dir () =
  OS.Dir.contents Fpath.(meta_dir / "revs") >>=
  C.iter (fun rev_file ->
    Sexplib.Sexp.load_sexp (Fpath.to_string rev_file) |> Obi.batch_of_sexp |> fun batch ->
    (* Generate the by-ocaml-version pages *)
    List.fold_left (fun acc (arch,distro,v) ->
      match arch,distro with
      |`X86_64,`Debian `V9 -> v::acc
      |_ -> acc) [] batch.Obi.res |>
    function
    | [hd] ->
        let rev = batch.Obi.rev in
        generate_by_version_for_rev ~rev ~distro:(`Debian `V9) ~arch:`X86_64 logs_uri hd >>= fun idx ->
        let srev = String.with_range ~len:8 rev in
        let dir = Fpath.(html_dir / "by-version" / srev) in
        OS.Dir.create ~path:true dir >>= fun _ ->
        let f = Fpath.(dir / "index.html") in
        write_html f idx
    | [] -> Error (`Msg "No results found for x86-64/debian-9")
    | _ -> Error (`Msg "Multiple results found for x86-64/debian-9")
  ) >>= fun () ->
  let index = Sexplib.Sexp.load_sexp (Fpath.(to_string (meta_dir / "index.sxp"))) |> Obi.index_of_sexp in
  let revs = List.sort (fun (_,a,_) (_,b,_) -> compare b a) index.Obi.revs in
  let index = { index with Obi.revs = revs } in
  let latest_rev = List.hd revs |> fun (r,_,_) -> String.with_range ~len:8 r in
  OS.Path.symlink ~force:true ~target:(Fpath.v latest_rev) Fpath.(html_dir / "by-version" / "latest") >>= fun () ->
  generate_index_by_version index >>= fun html ->
  write_html Fpath.(html_dir / "by-version" / "index.html") html
