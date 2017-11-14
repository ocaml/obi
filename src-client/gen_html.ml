module OV = Ocaml_version
module C = Dockerfile_cmd
module D = Dockerfile_distro
open Soup
open Astring
open Bos
open Rresult
open R.Infix

let top_navbar () =
  let e = create_element ~classes:["navbar";"fixed-top";"navbar-expand";"navbar-dark";"bg-dark"] "nav" in
  append_child e (create_element ~inner_text:"Obi" ~class_:"navbar-brand" ~attributes:["href","http://obi.ocamllabs.io/"] "a");
  let d = create_element ~classes:["collapse;";"navbar-collapse"] "div" in
  append_child e d;
  let ul = create_element ~classes:["navbar-nav";"mr-auto"] "ul" in
  append_child d ul;
  append_child ul (create_element ~classes:["nav-item"] "li" |> fun e -> append_child e (create_element ~class_:"nav-link" ~attributes:["href","http://obi.ocamllabs.io/"] ~inner_text:"Home" "a"); e);
  append_child ul (create_element ~classes:["nav-item"] "li" |> fun e -> append_child e (create_element ~class_:"nav-link" ~attributes:["href","http://obi.ocamllabs.io/triage/"] ~inner_text:"Triage" "a"); e);
  append_child ul (create_element ~classes:["nav-item"] "li" |> fun e -> append_child e (create_element ~class_:"nav-link" ~attributes:["href","#"] ~inner_text:"Logs" "a"); e);
  e

let idx () =
  let nav = top_navbar () in
  let css = "body { padding-top: 3rem; }" in
  Fmt.strf
    "\n<!doctype html>\n<html lang=\"en\">\n  <head>\n    <title class=\"template\"></title>\n    <meta charset=\"utf-8\">\n    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\">\n    <link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css\" integrity=\"sha384-PsH8R72JQ3SOdhVi3uxftmaW6Vc51MKb0q5P2rRUpPvrszuE4W1povHYgTpBfshb\" crossorigin=\"anonymous\">\n  <style>%s</style></head>\n  <body>\n  <div class=\"container\"><nav></nav></div> <br /> <main role=\"main\" class=\"container\">\n    <div class=\"col-9\">\n    <div class=\"row\">\n   <div class=\"intro\"></div> <div class=\"template\"></div>\n    </div>\n    </div>\n    </main>\n    <script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/js/bootstrap.min.js\" integrity=\"sha384-alpBpkh1PFOepccYVYDB4do5UnbKysX5WZXm3XxPqe5iKTfUKjNkCk9SaVuEZflJ\" crossorigin=\"anonymous\"></script>\n  </body>\n</html>\n" css |> parse |> fun b ->
  replace (b $ "nav") nav;
  b

let html_id_of_pkg_name pkg version =
  Fmt.strf "pkg-%s-%s" pkg (String.map (function |'.' -> '-'|x ->x) version)

let href href inner_text =
  create_element ~attributes:[("href", href)] ~inner_text "a"

let days_since date =
  let span =
    match Ptime.Span.of_float_s (Unix.gettimeofday () -. date) with
    | None -> failwith "invalid date"
    | Some d -> d in
  Fmt.strf "%a" Ptime.Span.pp span

let load_batch meta_dir rev =
  (* TODO use R.trap_exn *)
  Fpath.(meta_dir / "revs" / (rev ^ ".sxp")) |>
  Fpath.to_string |>
  Sexplib.Sexp.load_sexp |>
  Obi.batch_of_sexp

let load_index meta_dir =
  Fpath.(meta_dir / "index.sxp") |>
  Fpath.to_string |>
  Sexplib.Sexp.load_sexp |>
  Obi.index_of_sexp

let load_analysis meta_dir =
  Fpath.(meta_dir / "analysis.sxp") |>
  Fpath.to_string |>
  Sexplib.Sexp.load_sexp |>
  Obi.analysis_of_sexp

let write_html dir html =
  Logs.info (fun l -> l "Generating: %a" Fpath.pp dir);
  OS.File.write dir (Soup.to_string html)

open Obi.Analysis

let html_stats_for_pkgs elem (pkgs:Obi.pkg list) =
  (* TODO Factor out arch/distro selection *)
  let e = create_element elem in
  append_child e (create_text "Package build stats per different OCaml compiler are: ");
  append_child e (create_element "br");
  calculate_stats_for_pkgs pkgs |>
  List.iter (fun (ov,(ok,fail)) ->
    Fmt.strf "OCaml %a (<span class=\"text-success\">%d</a> / <span class=\"text-danger\">%d</span>)<br />" OV.pp ov ok fail |> fun s ->
    append_child e (parse s)
  ) |> fun () ->
  e
 
let html_flambda_error_406 rev pkgs =
  let ul = create_element ~classes:["list-unstyled";"row"] "ul" in
  let srev = String.with_range ~len:8 rev in
  flambda_errors_406 pkgs |>
  List.iter (fun (name, last_broken, versions) ->
    let e =
      List.map (fun (v,hash) -> Fmt.strf "<a href=\"http://obi.ocamllabs.io/logs/%s.txt\">%s</a> (<a href=\"http://obi.ocamllabs.io/by-version/%s/index.html#%s\">#</a>)" rev v srev (html_id_of_pkg_name name v)) versions |>
      String.concat ~sep:", " |>
      Fmt.strf "<b %s>%s</b>: %s" (if last_broken then "class=\"text-danger\"" else "") name |>
      parse in
    let li = create_element ~classes:["col-6"] "li" in
    append_child li e;
    append_child ul li
  ) |> fun e ->
  ul


let html_safe_string_error_406 rev pkgs = 
  let ul = create_element ~classes:["list-unstyled";"row"] "ul" in
  let srev = String.with_range ~len:8 rev in
  safe_string_errors_406 pkgs |>
  List.iter (fun (name, last_broken, versions) ->
    let e =
      List.map (fun (v,hash) -> Fmt.strf "<a href=\"http://obi.ocamllabs.io/logs/%s.txt\">%s</a> (<a href=\"http://obi.ocamllabs.io/by-version/%s/index.html#%s\">#</a>)" hash v srev (html_id_of_pkg_name name v)) versions |>
      String.concat ~sep:", " |>
      Fmt.strf "<b %s>%s</b>: %s" (if last_broken then "class=\"text-danger\"" else "") name |>
      parse in
    let li = create_element ~classes:["col-6"] "li" in
    append_child li e;
    append_child ul li
  ) |> fun e ->
  ul

let generate_triage index batches =
  let idx = idx () in
  let rev = index.Obi.most_recent_rev in
  let srev = String.with_range ~len:8 rev in
  let intro =
    let e =
      let e =
        create_element
          ~attributes:[("aria-label", "breadcrumb"); ("role", "navigation")]
          "nav"
      in
      let ol = create_element ~class_:"breadcrumb" "ol" in
      append_child e ol ;
      append_child ol
        (create_element ~classes:["breadcrumb-item"; "active"] "li" |> fun e -> append_child e (href "index.html" "Bulk Builds"); e);
      append_child ol
        (create_element ~classes:["breadcrumb-item"; "active"] ~inner_text:"Triage" "li");
      e
    in
 
    append_child e (create_element ~inner_text:"Triaging Build Failures" "h2");
    let foo = Fmt.strf "<p>We run analyses over the results of the bulk builds to correct for feature changes across OCaml compiler versions.  This page lists the active efforts that could use your help.  The full logs for the triage efforts here can be found at <a href=\"/by-version/%s/index.html\">by-version/%s</a>.</p>" srev srev |> parse in
    append_child e foo;
    append_child e (create_element ~inner_text:"OCaml 4.06.0 safe-string" ~id:"safe-string" "h4");
    let inner_text = "We have just released OCaml 4.06.0, with the safe-string feature.  The support was added via an optional command line flag across a couple of years to make strings immutable, and now the 4.06 release turns this on by default.  This has resulted in a number of package regressions which need to be fixed by releasing a new package which supports the feature, and by constraining older releases in opam to not be selected for compiler revisions 4.06.0 or greater." in
    append_child e (create_element ~inner_text "p");
    let inner_text = "In the list below, the red indicates that the latest version of the package is still broken with 4.06.0 as well.  We still need to fix older packages, but it is urgent to fix the immediate build breakages that are red." in
    append_child e (create_element ~inner_text "p");
    append_child e (html_safe_string_error_406 rev (find_pkgs_in_batch ~distro:(`Debian `V9) ~arch:`X86_64 ~rev batches));
    append_child e (create_element ~inner_text:"OCaml 4.06.0 flambda" ~id:"flambda" "h4");
    let inner_text = "Flambda is an experimental inliner for OCaml that should speed up programs. There should never be a program that fails with flambda and that works with normal OCaml, so this triage is highlighting failures where this is currently the case." in
    append_child e (create_element ~inner_text "p");
    append_child e (html_flambda_error_406 rev (find_pkgs_in_batch ~distro:(`Debian `V9) ~arch:`X86_64 ~rev batches));
    e
  in
  let title ="Opam bulk build results" in
  replace (idx $ "title.template") (create_element ~inner_text:title "title") ;
  replace (idx $ "div.intro") intro;
  Ok idx


let generate_root_index index batches =
  let idx = idx () in
  let intro =
    let e = create_element "div" in
    append_child e (create_element ~inner_text:"OCaml build infrastructure" "h2");
    let inner_text = "This site contains regular bulk builds of the opam package repository." in
    append_child e (create_element ~inner_text "p");
    let foo = Fmt.strf "<ul><li><b><a href=\"by-version/index.html\">Logs by Compiler Version</a></b>: contains regular snapshots of the full repository, built on Debian-9/x86_64 across recent OCaml compiler releases.</li><li><b>Logs by Distro</b>: Many Linux variants are built using containers, and these build logs help check portability of a release. <i>(coming soon)</i></li><li><b>By CPU architecture:</b> in addition to x86_64, we are also working on arm64 and ppc64le logs. <i>(coming soon)</i> </ul>" |> parse in
    append_child e foo;
    append_child e (create_element ~inner_text:"How to Contribute" "h3");
    let inner_text = "Your contributions and help are very welcome to improve the state of the opam package database.  Anyone can submit a pull request to the package repository to improve the metadata." in
    append_child e (create_element ~inner_text "p");
    let foo = Fmt.strf "<ul><li><b><a href=\"triage.html#safe-string\">4.06 safe-string migration</a></b> concerns moving the OCaml packages to support immutable strings, which are the new default in OCaml 4.06.0.</li><li><b><a href=\"triage.html#flambda\">flambda</a></b> looks for packages that do not compile using the experimental new flambda inliner.</li></ul>" |> parse in
    append_child e foo;
    e
  in
  let title ="Opam bulk build results" in
  replace (idx $ "title.template") (create_element ~inner_text:title "title") ;
  replace (idx $ "div.intro") intro;
  Ok idx

let generate_index_by_version index batches =
  let idx = idx () in
  let content =
    let ul = create_element ~classes:["list-group"] "div" in
    let _latest =
      let (rev, date, subj) = List.hd index.Obi.revs in
      let target = "latest/index.html" in
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
      let pkgs = (List.find (fun b -> b.Obi.rev = rev) batches) |> select_arch_distro ~arch:(`X86_64) ~distro:(`Debian `V9) in
      append_child li (html_stats_for_pkgs "small" pkgs);
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
        (create_element ~classes:["breadcrumb-item"; "active"] "li" |> fun e -> append_child e (href "../index.html" "Bulk Builds"); e);
      append_child ol
        (create_element ~classes:["breadcrumb-item"; "active"] ~inner_text:"By Compiler Version" "li");
      e
    in
    let e = Soup.parse (Fmt.strf "<h3>Builds Across Multiple Compiler Versions</h3><p>Snapshots of the opam <a href=\"https://github.com/ocaml/opam-repository\">package repository</a> are regularly built against recent versions of the OCaml compiler, including . This page lists the most recent builds, sorted by the git revision of the repository.</p>") in
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
      let vs = List.fold_left (fun a (ver,_) -> ver::a) [] pkg.versions |> List.rev in
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
          append_child vrow (create_element ~id:(html_id_of_pkg_name pkg_name pkg_version) ~inner_text:pkg_version "td") ;
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
        (create_element ~classes:["breadcrumb-item"; "active"] "li" |> fun e -> append_child e (href "../../index.html" "Bulk Builds"); e);
      append_child ol
        (create_element ~classes:["breadcrumb-item"; "active"] "li" |> fun e -> append_child e (href "../index.html" "By Compiler Version"); e);
      append_child ol
        (create_element ~classes:["breadcrumb-item"; "active"]
           ~attributes:[("aria-current", "page")] ~inner_text:short_hash "li") ;
      e
    in
    let e = Soup.parse (Fmt.strf "<p>These are the <a href=\"https://opam.ocaml.org/\">opam</a> bulk build results for the packages at <a href=\"https://github.com/ocaml/opam-repository\">opam-repository</a> revision <a href=\"https://github.com/ocaml/opam-repository/commit/%s\">%s</a>, built using %s on an %s architecture.</p>" short_hash short_hash (D.human_readable_string_of_distro distro) (OV.string_of_arch arch)) in
    append_child nav e;
    append_child nav (html_stats_for_pkgs "p" res);
    nav
  in
  replace (idx $ "div.intro") intro ;
  Ok idx

let generate logs_uri meta_dir logs_dir html_dir () =
  load_index meta_dir |> fun index ->
  List.map (fun (rev,_,_) -> load_batch meta_dir rev) index.Obi.revs |> fun batches ->
  C.iter (fun batch ->
     let rev = batch.Obi.rev in
     let arch = `X86_64 in
     let distro = `Debian `V9 in
     select_arch_distro ~arch ~distro batch |>
     generate_by_version_for_rev ~rev ~distro ~arch logs_uri >>= fun idx ->
     let srev = String.with_range ~len:8 rev in
     let dir = Fpath.(html_dir / "by-version" / srev) in
     OS.Dir.create ~path:true dir >>= fun _ ->
     let f = Fpath.(dir / "index.html") in
     write_html f idx
  ) batches >>= fun () ->
  let revs = List.sort (fun (_,a,_) (_,b,_) -> compare b a) index.Obi.revs in
  let index = { index with Obi.revs = revs } in
  let latest_rev = List.hd revs |> fun (r,_,_) -> String.with_range ~len:8 r in
  let latest_link = Fpath.(html_dir / "by-version" / "latest") in
  OS.File.delete ~must_exist:false latest_link >>= fun () ->
  (* unlink needed due to https://github.com/dbuenzli/bos/issues/75 *)
  OS.Path.symlink ~force:true ~target:(Fpath.v latest_rev) latest_link >>= fun () ->
  generate_index_by_version index batches >>= fun html ->
  write_html Fpath.(html_dir / "by-version" / "index.html") html >>= fun () ->
  generate_root_index index batches >>= fun html ->
  write_html Fpath.(html_dir / "index.html") html >>= fun () ->
  generate_triage index batches >>= fun html ->
  write_html Fpath.(html_dir / "triage.html") html
