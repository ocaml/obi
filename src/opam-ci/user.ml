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
  let other_distros = [`Alpine `V3_7; `Ubuntu `V18_04; `Fedora `V28]
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
    | Some {build_result=`Ok} -> `Ok
    | Some {build_result=`Fail _} -> `Fail
    | Some {build_result=`Depfail _} -> `Depfail
    | Some {build_result=`Uninstallable _} -> `Uninstallable
    | Some {build_result=`Solver_failure} -> `Solver_failure
    | Some {build_result=`No_sources _} -> `No_sources

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
    |`Depfail,_ | _,`Depfail -> `Depfail
    |`Uninstallable,_ | _,`Uninstallable -> `Uninstallable
    |`Solver_failure,_ | _,`Solver_failure -> `Solver_failure
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
    | `Unknown -> Fmt.(pf ppf "%a" (styled `Underline string) u)
    | `Ok -> Fmt.(pf ppf "%a" (styled `Green string) u)
    | `Uninstallable -> Fmt.(pf ppf "%a" string u)
    | `Solver_failure -> Fmt.(pf ppf "%a" (styled `Underline (styled `Magenta string)) u)
    | `No_sources -> Fmt.(pf ppf "%a" (styled `Blue string) u)
    | `Fail -> Fmt.(pf ppf "%a" (styled `Red (styled `Bold string)) u)
    | `Depfail -> Fmt.(pf ppf "%a" (styled `Yellow string) u)
 
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
    let col = function
      | `No_sources -> `Blue 
      | `Uninstallable -> `White 
      | `Unknown -> `Underline 
      | `Ok -> `Green 
      | `Depfail -> `Yellow
      | `Solver_failure -> `Magenta
      | `Fail -> `Red in
    let run fn u = Fmt.(pf ppf "%a" (styled (col (fn m)) string) u) in
    run A.test_safe_string U.ss;
    run A.test_flambda U.flambda;
    run A.test_ocaml406to7 U.release
end

type copts = {
  refresh: [`Local| `Poll| `Network];
}

type filters = {
  maintainers: string list;
  filters: [`All| `Failures| `Recent| `Variants of [ `Flambda | `RC | `SS ] ];
}

type params = {
  distro: D.t option;
  ov: OV.t option;
  arch: OV.arch option;
}

let filters maintainers filters =
  { maintainers; filters }

let copts refresh =
  { refresh }

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
    List.iter (fun s -> Fmt.pf ppf "%s@\n" s) (Wrapper.wrap w l)
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

let render_package_details ppf pkg name version {distro; ov; arch} =
  let open Obi.Index in
  (* Determine how many results match the result set *)
  let ms = List.fold_left (fun acc (v, ml) ->
    let metas = List.filter (fun m ->
      let distro_ok =
        match distro with
        | None -> true
        | Some d when d = m.params.distro -> true
        | Some _ -> false in
      let ov_ok =
        match ov with
        | None -> true
        | Some ov when ov = m.params.ov -> true
        | Some _ -> false in
      let arch_ok =
        match arch with
        | None -> true
        | Some arch when arch = m.params.arch -> true
        | Some _ -> false in
      let is_err =
        match m.build_result with
        | `Ok -> false
        | `Uninstallable _ -> false
        | _ -> true in
    is_err && arch_ok && ov_ok && distro_ok) ml in
    match version with
    | None -> (v,metas)::acc 
    | Some ver when ver = v -> (v,metas)::acc
    | Some _ -> acc) [] pkg.versions in
  let total_res = List.fold_left (fun a (_,ms) -> List.length ms + a) 0 ms in
  match total_res with
  | 0  ->
      Fmt.(pf ppf "%a: No failures found with these constraints\n%!" (styled `Bold string) name)
  | 1 ->
      List.iter (fun (version, m) ->
        List.iter (render_package_logs ppf version) m;
        List.iter (fun m ->
          Fmt.(pf ppf "@\n%a %a Dockerfile for %s %s %s:@\n" (styled `Bold (styled `Blue string)) "====>" (styled `Bold string) version
     (OV.to_string m.params.ov) (D.human_readable_string_of_distro m.params.distro) (OV.string_of_arch m.params.arch) );
          let dfile =
            let open Dockerfile in
            Dockerfile_opam.bulk_build "ocaml/opam2" m.params.distro m.params.ov m.rev @@
            run "opam depext -uiv %s.%s" name version
          in
          Fmt.pf ppf "%s@\n" (Dockerfile.string_of_t dfile)
        ) m
      ) ms
  | _ ->
      Fmt.(pf ppf "%a: multiple build failures found with different configuration parameters.@\nPlease refine the command to select exactly one of the following:@\n" (styled `Bold string) name);
      List.iter (fun (version, ms) ->
        List.iter (fun m ->
          Fmt.(pf ppf "  opam-ci logs %s.%s --compiler=%s --arch=%s --distro=%s\n%!" name version (OV.to_string m.params.ov) (OV.string_of_arch m.params.arch) (D.tag_of_distro m.params.distro))
        ) ms) ms

let show_status {refresh} {maintainers; filters} () =
  let open Obi.Index in
  Repos.init ~refresh () >>= fun pkgs ->
  let ppf = Fmt.stdout in
  let pkgs = List.sort (fun a b -> String.compare a.name b.name) pkgs in
  List.iter (fun pkg ->
    if check_maintainer ~maintainers pkg then
      render_package ppf ~filters pkg
  ) pkgs;
  Ok ()

let show_logs pkg {refresh} params () =
  let open Obi.Index in
  let name, version =
    match String.cut ~sep:"." pkg with
    | None -> pkg, None (* No version string *)
    | Some (name, version) -> name, Some version
  in
  let ppf = Fmt.stdout in
  Repos.init ~refresh () >>= fun pkgs ->
  match List.find_opt (fun p -> p.name = name) pkgs with
  | None -> Error (`Msg (Fmt.strf "Package %s not found" pkg))
  | Some pkg ->
     render_package_details ppf pkg name version params;
     Ok ()

