open Cmdliner
open Rresult
open R.Infix
open Astring
open Bos
module C = Dockerfile_cmd
module OV = Ocaml_version

let setup_logs = C.setup_logs ()

let fpath = Arg.conv ~docv:"PATH" (Fpath.of_string, Fpath.pp)

let meta_dir_t =
  let doc = "metadata output directory" in
  let open Arg in
  value & opt fpath (Fpath.v "_metadata")
  & info ["metadata-dir"] ~docv:"METADATA_DIR" ~doc

let logs_dir_t =
  let doc = "build logs output directory" in
  let open Arg in
  value & opt fpath (Fpath.v "_build_logs")
  & info ["buildlogs-dir"] ~docv:"BUILDLOGS_DIR" ~doc

let html_dir_t =
    let doc = "output dir for html results site" in
    let open Arg in 
    value & pos 0 fpath (Fpath.v "_html")
    & info [] ~docv:"HTML_OUTPUT_DIR" ~doc

let opam_repo_rev_t =
  let doc = "opam repo git rev" in
  let open Arg in
  required & opt (some string) None
  & info ["opam-repo-rev"] ~docv:"OPAM_REPO_REV" ~doc

let logs_cmd =
  let input_t =
    let doc = "input dir for cluster logs" in
    let open Arg in 
    value & pos 0 fpath (Fpath.v "_results")
    & info [] ~docv:"INPUT_DIR" ~doc in
  let doc = "import build logs after a successful build" in
  let exits = Term.default_exits in
  ( (let open Term in
    term_result
      (const Import.gather_logs $ meta_dir_t $ logs_dir_t $ input_t $ setup_logs))
  , Term.info "import-logs" ~doc ~exits )

let gen_html_cmd =
  let logs_uri_t = 
    let doc ="Base URi for build logs (stored by hash)" in
    let open Arg in
    value & opt string "http://ci-logs.ocamllabs.io/" &
    info ["logs-uri"] ~docv:"LOGS_URI" ~doc in
  let doc = "generate html site from results metadata" in
  let open Arg in
  let exits = Term.default_exits in
  ( (let open Term in
    term_result
    (const Gen_html.generate $ logs_uri_t $ meta_dir_t $ logs_dir_t $ html_dir_t $ setup_logs))
  , Term.info "gen-html" ~doc ~exits )

let gen_index_cmd =
  let doc = "sync index revision information from opam repository" in
  let open Arg in
  let exits = Term.default_exits in
  ( (let open Term in
    term_result
    (const Import.generate_index $ meta_dir_t $ setup_logs))
  , Term.info "gen-index" ~doc ~exits)

let default_cmd =
  let doc = "triage OCaml Build Infrstructure build logs" in
  let sdocs = Manpage.s_common_options in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
  , Term.info "obi" ~version:"v1.0.0" ~doc ~sdocs )

let cmds =
  [ logs_cmd; gen_html_cmd; gen_index_cmd ]

let () = Term.(exit @@ eval_choice default_cmd cmds)
