open Bos
open Rresult
open Astring
open Printf

module U = struct
  let c3 = "③"
  let c4 = "④"
  let c5 = "⑤"
  let c6 = "⑥"
  let c7 = "⑦"
  let c8 = "⑧"
end

module A = struct
  open Obi.Index
  (* Latest stable *)
  let stable ms =
    List.filter (fun m ->
      (* TODO need to refresh this without a dependency on dockerfile-distro *)
      m.params.distro = `Debian `V9
    ) ms
end

let check_maintainer pkg =
  let open Obi.Index in
  (* TODO merge maintainer into main pkg record *)
  List.fold_left (fun a (_,m) ->
   if List.exists (fun m -> m.maintainer = "anil@recoil.org") m then true else a) false pkg.versions

let render_package pkg =
  let open Obi.Index in
  printf "%s\n" pkg.name

let show_status maintainer () =
  let open Obi.Index in
  Repos.init () >>= fun pkgs ->
  let pkgs = List.sort (fun a b -> String.compare a.name b.name) pkgs in
  List.iter (fun pkg ->
    if check_maintainer pkg then
      render_package pkg
  ) pkgs;
  Ok ()

open Cmdliner
let setup_logs =
  let setup_log style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ()) in
  let global_option_section = "COMMON OPTIONS" in
  Term.(const setup_log
    $ Fmt_cli.style_renderer ~docs:global_option_section ()
    $ Logs_cli.level ~docs:global_option_section ())

let status_cmd =
  let doc = "obi status" in
  let exits = Term.default_exits in
  let filter_t =
    let doc = "maintainer filter" in
    let open Arg in
    value & opt string ""
    & info ["filter"; "f"] ~docv:"FILTER" ~doc
  in
  let man =
    [ `S Manpage.s_description
    ; `P "obi status." ]
  in
  ( Term.(term_result (const show_status $ filter_t $ setup_logs))
  , Term.info "status" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )

let default_cmd =
  let doc = "obi" in
  let sdocs = Manpage.s_common_options in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
  , Term.info "obi" ~version:"v1.0.0" ~doc ~sdocs )


let cmds = [ status_cmd ]

let () = Term.(exit @@ eval_choice default_cmd cmds)

