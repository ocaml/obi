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

  let tick = "✓ "
  let cross = "✘ "

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
      m.params.distro = base_distro
    ) ms

  type res = [ `Ok | `Fail | `Uninstallable | `Unknown ]

  let classify m =
    match m with 
    | None -> `Unknown
    | Some {build_result=`Exited 0} -> `Ok
    | Some {build_result=`Exited _} -> `Fail
    | Some {build_result=`Signaled _} -> `Fail
    | Some {build_result=`Uninstallable _} -> `Uninstallable
    | Some {build_result=`No_sources _} -> `No_sources

  let is_success m =
    m.build_result = `Exited 0

  let latest_version pkg =
    List.sort (fun a b ->
      Obi.VersionCompare.compare (fst b) (fst a)) pkg.versions |> List.hd

  let test_two_versions a b m =
    let ss = find ~ov:a m in
    let ss' = classify ss in
    let uss = find ~ov:b m in
    let uss' = classify uss in
    match ss', uss' with
    |`Fail, `Ok | `Ok, `Fail -> `Fail
    |`Ok, `Ok -> `Ok
    |`Fail, `Fail -> `Ok
    |`Uninstallable,_ | _,`Uninstallable -> `Uninstallable
    |`No_sources,_ | _,`No_sources -> `No_sources
    |`Unknown,_ | _,`Unknown -> `Unknown

  let test_safe_string m =
    test_two_versions ov_stable ov_stable_uss m

  let test_flambda m =
    test_two_versions ov_stable ov_stable_fl m

  let test_ocaml406to7 m =
    test_two_versions ov_stable ov_rc m

  let is_fail = function |`Fail |`No_sources -> true | _ -> false
  (* There are any failures *)
  let has_fails m =
    List.exists (fun m -> classify (Some m) |> is_fail) m

  let has_variant_fails ty m =
    match ty with
    |`Flambda -> test_flambda m |> is_fail
    |`SS -> test_safe_string m |> is_fail
    |`RC -> test_ocaml406to7 m |> is_fail

  let any_variant_fails m =
    List.exists (fun ty -> has_variant_fails ty m) [`Flambda;`SS;`RC]

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

  let render_classify ppf fn m u =
    match fn m |> A.classify with
    | `Unknown -> Fmt.(pf ppf "%a" (styled `Yellow string) u)
    | `Ok -> Fmt.(pf ppf "%a" (styled `Green string) u)
    | `Uninstallable -> Fmt.(pf ppf "%a" string u)
    | `No_sources -> Fmt.(pf ppf "%a" (styled `Blue string) u)
    | `Fail -> Fmt.(pf ppf "%a" (styled `Red (styled `Bold string)) u)
 
  let compilers ppf (m:metadata list) =
    List.iter (fun ov ->
      u_of_ov (OV.to_string ov) |>
      render_classify ppf (A.find ~ov) m
    ) A.ovs 

  let distros ppf m =
    List.iter (fun distro ->
      u_of_distro distro |>
      render_classify ppf (A.find ~distro) m
    ) A.distros

  let arches ppf m =
    List.iter (fun arch ->
      u_of_arch arch |>
      render_classify ppf (A.find ~arch) m
    ) OV.arches

  let variants ppf m =
    let col = function `No_sources -> `Blue |`Uninstallable -> `White | `Unknown -> `Yellow | `Ok -> `Green | `Fail -> `Red in
    let run fn u = Fmt.(pf ppf "%a" (styled (col (fn m)) string) u) in
    run A.test_safe_string U.ss;
    run A.test_flambda U.flambda;
    run A.test_ocaml406to7 U.release
end

type copts = {
  maintainers: string list;
  filters: [`All|`Failures|`Recent|`Variants of [ `Flambda | `RC | `SS ] ];
  refresh: [`Local|`Poll|`Network];
}

type params = {
  distro: D.t option;
  ov: OV.t option;
  arch: OV.arch option;
}

let copts maintainers filters refresh =
  { maintainers; filters; refresh }

let params distro ov arch =
  { distro ; ov; arch }

let check_maintainer ~maintainers pkg =
  let open Obi.Index in
  match maintainers with
  | [] -> true
  | _ ->
    let l = List.map String.Ascii.lowercase (pkg.maintainers @ pkg.tags) in
    List.exists (fun sub ->
      List.exists (fun p ->
        String.find_sub ~sub p <> None) l) maintainers

let render_package_version ppf (version,metadata) =
  Fmt.(pf ppf "%14s  " version);
  S.compilers ppf metadata;
  Fmt.(pf ppf "  ");
  S.distros ppf metadata;
  Fmt.(pf ppf "  ");
  S.arches ppf metadata;
  Fmt.(pf ppf "  ");
  S.variants ppf metadata;
  Fmt.(pf ppf "@\n%!")

let render_package_logs ppf version metadata =
  let open Obi.Index in
  let p = metadata.params in
  match metadata.log with
  | [] -> ()
  | logs ->
    Fmt.(pf ppf "@\n%a %a %s %s %s:@\n" (styled `Bold (styled `Blue string)) "====>" (styled `Bold string) version
     (OV.to_string p.ov) (D.human_readable_string_of_distro p.distro) (OV.string_of_arch p.arch) );
  let w = Wrapper.make ~initial_indent:" " ~subsequent_indent:" " ~drop_whitespace:true 100 in
  List.iter (fun l ->
    List.iter print_endline (Wrapper.wrap w l)
  ) metadata.log

let render_packages ppf name pkgs =
  List.iter (fun v ->
    Fmt.pf ppf "%35s %!" name;
    render_package_version ppf v) pkgs

let render_package ppf ~filters pkg =
  let open Obi.Index in
  match filters with
  | `All -> render_packages ppf pkg.name pkg.versions
  | `Failures ->
    List.filter (fun (_, m) -> A.has_fails m) pkg.versions |>
    render_packages ppf pkg.name
  | `All_variants ->
    List.filter (fun (_, m) -> A.any_variant_fails m) pkg.versions |>
    render_packages ppf pkg.name
  | `Variants ty ->
    List.filter (fun (_, m) -> A.has_variant_fails ty m) pkg.versions |>
    render_packages ppf pkg.name
  | `Recent ->
    render_packages ppf pkg.name [A.latest_version pkg]

let render_package_details ppf pkg =
  let open Obi.Index in
  Fmt.(pf ppf "%a:@\n" (styled `Bold string) pkg.name);
  List.iter (fun (version, metadata) ->
    render_package_version ppf (version, metadata);
    List.iter (render_package_logs ppf version) metadata;
    Fmt.(pf ppf "@\n");
  ) pkg.versions

let show_status {maintainers; filters; refresh} () =
  let open Obi.Index in
  Repos.init ~refresh () >>= fun pkgs ->
  let ppf = Fmt.stdout in
  let pkgs = List.sort (fun a b -> String.compare a.name b.name) pkgs in
  List.iter (fun pkg ->
    if check_maintainer ~maintainers pkg then
      render_package ppf ~filters pkg
  ) pkgs;
  Ok ()

let show_logs pkg {refresh} {distro;ov;arch} () =
  let open Obi.Index in
  (* TODO split on version *)
  let ppf = Fmt.stdout in
  Repos.init ~refresh () >>= fun pkgs ->
  match List.find_opt (fun p -> p.name = pkg) pkgs with
  | None -> Error (`Msg "Package not found")
  | Some pkg ->
     render_package_details ppf pkg;
     Ok ()

open Cmdliner
let setup_logs =
  let setup_log style_renderer level =
    let style_renderer = match style_renderer with
      | None -> `Ansi_tty
      | Some t -> t in
    Fmt_tty.setup_std_outputs ~style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ()) in
  let global_option_section = "COMMON OPTIONS" in
  Term.(const setup_log
    $ Fmt_cli.style_renderer ~docs:global_option_section ()
    $ Logs_cli.level ~docs:global_option_section ())

let copts_t =
  let maintainer =
    let doc = "Match packages via a case-insensitive substring check on the $(b,maintainer) and $(b,tags) fields in the opam metadata. $(docv) can be repeated multiple times to include more filters." in
    let open Arg in
    value & opt_all string []
    & info ["maintainer"; "m"] ~docv:"MAINTAINER" ~doc
  in
  let filters =
    let doc = "Filter the list of packages displayed. $(docv) defaults to $(i,failures) to show all packages with errors. $(i,recent) will show results for the latest version of each package. $(i,variants) will list packages that regress with a compiler variant such as safe-string or flambda. $(i,variants:fl), $(i,variants:rc) and $(i,variants:ss) will show failures for just the flambda, release-candidate or safe-string tests respectively. $(i,all) will show all known results for all versions including successes." in
    let open Arg in
    let term = Arg.enum ["all",`All; "failures",`Failures; "recent",`Recent; "variants:ss",`Variants `SS; "variants:fl", `Variants `Flambda; "variants:rc", `Variants `RC] in
    value & opt term `Failures & info ["filter";"f"] ~docv:"FILTERS" ~doc
  in
  let refresh =
    let doc = "How to refresh status logs. $(docv) defaults to $(i,poll) which pulls from GitHub every hour. $(i,network) will force the metadata to be pulled, and $(i,local) will only use the cached version locally." in
    let open Arg in
    let term = Arg.enum ["local",`Local;"poll",`Poll;"network",`Network] in
    value & opt term `Poll & info ["refresh"] ~docv:"REFRESH_LOGS" ~doc
  in
  let open Term in
  const copts $ maintainer $ filters $ refresh

let param_t =
  let distro =
    let doc = "List only logs relating to this distribution" in
    let open Arg in
    let term = conv ~docv:"DISTRO"
     ((fun d -> match D.distro_of_tag d with Some r -> Ok r | None -> Error (`Msg ("unknown distribution " ^ d))),
      (fun ppf d -> Fmt.pf ppf "%s" (D.tag_of_distro d))) in
    value & opt (some term) None  & info ["distro"] ~docv:"DISTRO" ~doc
  in
  let ov =
    let doc = "List only logs relating to this OCaml compiler version" in
    let open Arg in
    let term = conv ~docv:"COMPILER" (OV.of_string,OV.pp) in
    value & opt (some term) None & info ["compiler"] ~doc in
  let arch =
    let doc = "List only logs relating to this CPU architecture" in
    let open Arg in
    let term = enum ["amd64", `X86_64; "arm64", `Aarch64; "ppc64le", `Ppc64le] in
    value & opt (some term) None & info ["arch"] ~doc in
  let open Term in
  const params $ distro $ ov $ arch

let status_cmd =
  let doc = "summary of builds across compilers, OS and CPUs" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "The status view shows a panel of icons that represent different combinations of ways to build opam packages. From left to right, these are:"
    ; `I ("$(b,Compiler)", "The circled numbers represent OCaml compiler versions (a circled 6 is OCaml 4.06, a circled 7 is 4.07, and so on). ")
    ; `I ("$(b,Distro)","The square letters indicate different OS distributions. $(i,D) is Debian, $(i,F) is Fedora, $(i,A) is Alpine, $(i,U) is Ubuntu and $(i,O) is OpenSUSE.")
    ; `I ("$(b,CPU Architecture)", "The small circled letters represent different CPU architectures. $(i,x) represents x86_64, $(i,a) is arm64 and $(i,p) is PowerPC64LE.")
    ; `P "Some compiler variants are also tested to track down specific problems, shown by the icons to the far right of the display."
    ; `I ("$(b,safe-string)", "The $(i,ss) icon is for 'safe-string' failures, which would happen in OCaml 4.06 due to the switch to immutable strings.")
    ; `I ("$(b,flambda)","The $(i,fl) icon is for packages that fail to compile with the flambda variant of the compiler.")
    ; `I ("$(b,release-candidate)","The $(i,flag) icon is for packages that fail to compile with the latest release candidate of OCaml; this is useful to figure out how much of the ecosystem works with a soon-to-be-released compiler.")
    ]
  in
  ( Term.(term_result (const show_status $ copts_t $ setup_logs))
  , Term.info "status" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )

let logs_cmd =
  let doc = "obi logs" in
  let exits = Term.default_exits in
  let pkg_t = Arg.(required & pos 0 (some string) None & info [] ~doc) in
  let man =
    [ `S Manpage.s_description
    ; `P "TODO" ]
  in
  ( Term.(term_result (const show_logs $ pkg_t $ copts_t $ param_t $ setup_logs))
  , Term.info "logs" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )


let default_cmd =
  let doc = "opam 2.0 package manager continuous integration interface" in
  let sdocs = Manpage.s_common_options in
  let man =
    [ `S Manpage.s_description
    ; `P "The $(tname) tool provides an interface to the opam 2.0 continuous
  integration cluster, which regularly rebuilds the package repository across
  a variety of OCaml compiler versions, operating system distributions and 
  CPU architectures.  These builds are done regularly in remote infrastructure and
  the results are pushed to a metadata repository where they are fetched by this
  command to give you a summary of the status of your own packages."
   ; `P "The $(i,opam-ci status) command shows a dashboard of the build results
   across this matrix. Packages can be filtered by maintainer substrings or tag names in the
    opam package description, so you see only those relevant to you.  Once you
    find some errors, the $(i,opam-ci logs) command will show you the build errors
    so you can fix them.  Finally, the $(i,opam-ci repro) command can generate
    a Dockerfile of the precise build to reproduce the environment locally for you."
   ; `P "To get started, try these commands with the maintainer argument replaced with your own information or tags:"
   ; `P "opam-ci status -m org:mirage | less -R"; `Noblank
   ; `P "opam-ci status -m anil --filter=all | less -R"; `Noblank
   ; `P "opam-ci logs mirage-xen"
   ; `P "See the $(b,--help) pages for the subcommands below for more detailed information."
   ] in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
  , Term.info "opam-ci" ~version:"v1.0.0" ~doc ~sdocs ~man)


let cmds = [ status_cmd; logs_cmd ]

let () = Term.(exit @@ eval_choice default_cmd cmds)
