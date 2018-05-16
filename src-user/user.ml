open Bos
open Rresult
open Astring
open Printf
module OV = Ocaml_version
module D = Dockerfile_distro

module U = struct
  let c3 = "③ "
  let c4 = "④ "
  let c5 = "⑤ "
  let c6 = "⑥ "
  let c7 = "⑦ "
  let c8 = "⑧ "

  let of_ov = function
  | "4.03" -> c3
  | "4.04" -> c4
  | "4.05" -> c5
  | "4.06" -> c6
  | "4.07" -> c7
  | "4.08" -> c8
  | _ -> "?"

  let tick = "✓"
  let cross = "✘"
end

module A = struct
  open Obi.Index
  let ovs = List.map OV.of_string_exn ["4.03";"4.04";"4.05";"4.06";"4.07";"4.08"] 
  let ov_stable = OV.of_string_exn "4.06"
  let ov_stable_uss = OV.of_string_exn "4.06+default-unsafe-string"
  let ov_stable_fl = OV.of_string_exn "4.06+flambda"
  let base_distro = `Debian `V9
  let other_distros = [`Alpine `V3_7; `Ubuntu `V18_04; `Fedora `V27]

  let find ?(distro=base_distro) ?(ov=ov_stable) ?(arch=`X86_64) (m:metadata list) =
    List.find_opt (fun m -> m.params.distro=distro && m.params.ov=ov && m.params.arch=arch) m

 (* Latest stable *)
  let stable ms =
    List.filter (fun m ->
      (* TODO need to refresh this without a dependency on dockerfile-distro *)
      m.params.distro = base_distro
    ) ms

  let is_success m =
    m.build_result = `Exited 0

  let test_two_versions a b m =
    let ss = find ~ov:a m in
    let uss = find ~ov:b m in
    match ss, uss with
    |Some ss, Some uss -> begin
      match (is_success ss), is_success uss with
      | false, true -> Some false
      | true, false -> Some false
      | _ -> Some true
    end
    |_ -> None

  let test_safe_string m =
    test_two_versions ov_stable ov_stable_uss m

  let test_flambda m =
    test_two_versions ov_stable ov_stable_fl m

  (* Distros that failed where the Debian version didnt *)
  let find_distro_fails m =
    match find ~distro:base_distro m with
    | Some m' when is_success m' ->
      List.fold_left (fun acc distro ->
        match find ~distro m with
        | Some m when not (is_success m) -> distro::acc
        | _ -> acc) [] other_distros
    | _ -> []
end

module S = struct
  open Obi.Index

  let compilers ppf (m:metadata list) =
    List.iter (fun ov ->
      let u = U.of_ov (OV.to_string ov) in
      match A.find ~ov m with
      | None -> Fmt.(pf ppf "%a" (styled `Yellow string) u)
      | Some m when m.build_result = `Exited 0 -> Fmt.(pf ppf "%a" (styled `Green string) u)
      | Some m -> Fmt.(pf ppf "%a" (styled `Red string) u)
    ) A.ovs 

  let variants ppf m =
    if A.test_safe_string m = Some false then
      Fmt.(pf ppf "%a" (styled `Red string) " ss");
    if A.test_flambda m = Some false then
      Fmt.(pf ppf "%a" (styled `Red string) " fl");
    List.iter (fun distro ->
      Fmt.(pf ppf "%a" (styled `Red string) (" " ^ (D.tag_of_distro distro)))
    ) (A.find_distro_fails m)
end

let check_maintainer pkg =
  let open Obi.Index in
  (* TODO merge maintainer into main pkg record *)
  List.fold_left (fun a (_,m) ->
   if List.exists (fun m -> m.maintainer = "anil@recoil.org") m then true else a) false pkg.versions

let render_package pkg =
  let open Obi.Index in
  printf "%s:\n" pkg.name;
  List.iter (fun (version,metadata) ->
    (* OCaml version builds *)
    Fmt.(pf stdout "@[%10s " version);
    S.compilers Fmt.stdout metadata;
    S.variants Fmt.stdout metadata;
    Fmt.(pf stdout "@]@\n");
  ) pkg.versions

let show_status maintainer () =
  let open Obi.Index in
  Repos.init () >>= fun pkgs ->
  let pkgs = List.sort (fun a b -> String.compare a.name b.name) pkgs in
  List.iter (fun pkg ->
    if check_maintainer pkg then
      render_package pkg
  ) pkgs;
  Ok ()

let show_errors pkg () =
  let open Obi.Index in
  (* TODO split on version *)
  Repos.init () >>= fun pkgs ->
  match List.find_opt (fun p -> p.name = pkg) pkgs with
  | None -> Error (`Msg "Package not found")
  | Some pkg ->
      render_package pkg;
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

let errors_cmd =
  let doc = "obi errors" in
  let exits = Term.default_exits in
  let pkg_t = Arg.(required & pos 0 (some string) None & info [] ~doc) in
  let man =
    [ `S Manpage.s_description
    ; `P "obi errors." ]
  in
  ( Term.(term_result (const show_errors $ pkg_t $ setup_logs))
  , Term.info "errors" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )


let default_cmd =
  let doc = "obi" in
  let sdocs = Manpage.s_common_options in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
  , Term.info "obi" ~version:"v1.0.0" ~doc ~sdocs )


let cmds = [ status_cmd; errors_cmd ]

let () = Term.(exit @@ eval_choice default_cmd cmds)

