(* Copyright (c) 2018 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

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

  let tick = "✓"
  let cross = "✘"

  let debian = "🄳 "
  let fedora = "🄵 "
  let ubuntu = "🅄 "
  let opensuse = "🅂 "
  let alpine = "🄰 "

  let flambda ="ﬂ "
  let ss = "∬ "
  let release = "⚐ "

  let amd64 = "ⓧ "
  let arm64 = "ⓐ "
  let ppc64 = "ⓟ "
end

module A = struct
  open Obi.Index
  let ovs = List.map OV.of_string_exn ["4.03";"4.04";"4.05";"4.06";"4.07";"4.08"] 
  let ov_stable = OV.of_string_exn "4.06"
  let ov_rc = OV.of_string_exn "4.07"
  let ov_stable_uss = OV.of_string_exn "4.06+default-unsafe-string"
  let ov_stable_fl = OV.of_string_exn "4.06+flambda"
  let base_distro = `Debian `V9
  let other_distros = [`Alpine `V3_7; `Ubuntu `V18_04; `Fedora `V27]
  let distros = base_distro :: other_distros

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

  let latest_version pkg =
    List.sort (fun a b ->
      Obi.VersionCompare.compare (fst b) (fst a)) pkg.versions |> List.hd

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

  let test_ocaml406to7 m =
    test_two_versions ov_stable ov_rc m

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

  let u_of_ov =
    let open U in function
    | "4.03" -> c3
    | "4.04" -> c4
    | "4.05" -> c5
    | "4.06" -> c6
    | "4.07" -> c7
    | "4.08" -> c8
    | _ -> "?"

  let u_of_distro =
    let open U in function
    | `Debian _ -> debian
    | `Fedora _ -> fedora
    | `OpenSUSE _ -> opensuse
    | `Alpine _ -> alpine
    | `Ubuntu _ -> ubuntu
    | _ -> "?"

  let u_of_arch =
    let open U in function
    | `X86_64 -> amd64
    | `Aarch64 -> arm64
    | `Ppc64le -> ppc64

  let compilers ppf (m:metadata list) =
    List.iter (fun ov ->
      let u = u_of_ov (OV.to_string ov) in
      match A.find ~ov m with
      | None -> Fmt.(pf ppf "%a" (styled `Yellow string) u)
      | Some m when m.build_result = `Exited 0 -> Fmt.(pf ppf "%a" (styled `Green string) u)
      | Some m -> Fmt.(pf ppf "%a" (styled `Red string) u)
    ) A.ovs 

  let distros ppf m =
    List.iter (fun distro ->
      let u = u_of_distro distro in
      match A.find ~distro m with
      | None -> Fmt.(pf ppf "%a" (styled `Yellow string) u)
      | Some m when m.build_result = `Exited 0 -> Fmt.(pf ppf "%a" (styled `Green string) u)
      | Some m -> Fmt.(pf ppf "%a" (styled `Red string) u)
    ) A.distros

  let arches ppf m =
    List.iter (fun arch ->
      let u = u_of_arch arch in
      match A.find ~arch m with
      | None -> Fmt.(pf ppf "%a" (styled `Yellow string) u)
      | Some m when m.build_result = `Exited 0 -> Fmt.(pf ppf "%a" (styled `Green string) u)
      | Some m -> Fmt.(pf ppf "%a" (styled `Red string) u)
    ) OV.arches

  let variants ppf m =
    let col = function None -> `Yellow | Some true -> `Green | Some false -> `Red in
    let run fn u = Fmt.(pf ppf "%a" (styled (col (fn m)) string) u) in
    run A.test_safe_string U.ss;
    run A.test_flambda U.flambda;
    run A.test_ocaml406to7 U.release
end

type copts = {
  maintainers: string list;
  all_versions: bool;
}

let copts maintainers all_versions =
  { maintainers; all_versions }

let check_maintainer ~maintainers pkg =
  let open Obi.Index in
  let l = List.map String.Ascii.lowercase (pkg.maintainers @ pkg.tags) in
  List.exists (fun sub ->
    List.exists (fun p ->
      String.find_sub ~sub p <> None) l) maintainers

let render_package_version (version,metadata) =
  Fmt.(pf stdout "%10s " version);
  S.compilers Fmt.stdout metadata;
  Fmt.(pf stdout "  ");
  S.distros Fmt.stdout metadata;
  Fmt.(pf stdout "  ");
  S.arches Fmt.stdout metadata;
  Fmt.(pf stdout "  ");
  S.variants Fmt.stdout metadata;
  Fmt.(pf stdout "@\n%!")

let render_package_logs version metadata =
  let open Obi.Index in
  let p = metadata.params in
  match metadata.log with
  | [] -> ()
  | logs ->
    Fmt.(pf stdout "@\n%a %a %s %s %s:@\n" (styled `Bold (styled `Blue string)) "====>" (styled `Bold string) version
     (OV.to_string p.ov) (D.human_readable_string_of_distro p.distro) (OV.string_of_arch p.arch) );
  let w = Wrapper.make ~initial_indent:" " ~subsequent_indent:" " ~drop_whitespace:true 100 in
  List.iter (fun l ->
    List.iter print_endline (Wrapper.wrap w l)
  ) metadata.log

let render_package ~all_versions pkg =
  let open Obi.Index in
  match all_versions with
  | true ->
    printf "%s:\n%!" pkg.name;
    List.iter render_package_version pkg.versions
  | false ->
    let version = A.latest_version pkg in
    printf "%30s %!" pkg.name;
    render_package_version version;
    printf "%!"

let render_package_details pkg =
  let open Obi.Index in
  Fmt.(pf stdout "%a:@\n" (styled `Bold string) pkg.name);
  List.iter (fun (version, metadata) ->
    render_package_version (version, metadata);
    List.iter (render_package_logs version) metadata;
    Fmt.(pf stdout "@\n");
  ) pkg.versions

let show_status {maintainers; all_versions} () =
  let open Obi.Index in
  Repos.init () >>= fun pkgs ->
  let pkgs = List.sort (fun a b -> String.compare a.name b.name) pkgs in
  List.iter (fun pkg ->
    if check_maintainer ~maintainers pkg then
      render_package ~all_versions pkg
  ) pkgs;
  Ok ()

let show_logs pkg () =
  let open Obi.Index in
  (* TODO split on version *)
  Repos.init () >>= fun pkgs ->
  match List.find_opt (fun p -> p.name = pkg) pkgs with
  | None -> Error (`Msg "Package not found")
  | Some pkg ->
     render_package_details pkg;
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

let copts_t =
  let maintainer =
    let doc = "List of maintainer strings to filter packages on" in
    let open Arg in
    value & opt_all string []
    & info ["maintainer"; "m"] ~docv:"MAINTAINER" ~doc
  in
  let all_versions =
    let doc = "Show all versions of packages" in
    let open Arg in
    value & flag
    & info ["all-versions";"a"] ~docv:"ALL_VERSIONS" ~doc
  in
  let open Term in
  const copts $ maintainer $ all_versions

let status_cmd =
  let doc = "obi status" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "obi status." ]
  in
  ( Term.(term_result (const show_status $ copts_t $ setup_logs))
  , Term.info "status" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )

let logs_cmd =
  let doc = "obi logs" in
  let exits = Term.default_exits in
  let pkg_t = Arg.(required & pos 0 (some string) None & info [] ~doc) in
  let man =
    [ `S Manpage.s_description
    ; `P "obi logs." ]
  in
  ( Term.(term_result (const show_logs $ pkg_t $ setup_logs))
  , Term.info "logs" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )


let default_cmd =
  let doc = "obi" in
  let sdocs = Manpage.s_common_options in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
  , Term.info "obi" ~version:"v1.0.0" ~doc ~sdocs )


let cmds = [ status_cmd; logs_cmd ]

let () = Term.(exit @@ eval_choice default_cmd cmds)

