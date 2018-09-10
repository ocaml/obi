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

module L = Dockerfile_linux
module D = Dockerfile_distro
module C = Dockerfile_cmd
module G = Dockerfile_gen
module O = Dockerfile_opam
module OV = Ocaml_version
open Bos
open Rresult
open R.Infix

type copts = {staging_hub_id: string; prod_hub_id: string; results_dir: Fpath.t}

let copts staging_hub_id prod_hub_id results_dir =
  {staging_hub_id; prod_hub_id; results_dir}

let arches = OV.arches

type build_t = {ov: Ocaml_version.t; distro: D.t}

let docs {prod_hub_id; _} =
  let distros ds =
    List.map
      (fun distro ->
        let name = D.human_readable_string_of_distro distro in
        let tag = D.tag_of_distro distro in
        let arches =
          String.concat " "
            (List.map OV.string_of_arch
               (D.distro_arches OV.Releases.latest distro))
        in
        Fmt.strf "| %s | `%s` | %s | `docker run %s:%s`" name tag arches
          prod_hub_id tag )
      ds
    |> String.concat "\n"
  in
  let latest_distros = distros D.latest_distros in
  let active_distros = distros (D.active_distros `X86_64) in
  let dev_versions_of_ocaml =
    String.concat " " (List.map OV.to_string OV.Releases.dev)
  in
  let intro =
    Fmt.strf
      {|# OCaml Container Infrastructure

This repository contains a set of [Docker](http://docker.com) container definitions
for various combination of [OCaml](https://ocaml.org) and the
[OPAM](https://opam.ocaml.org) package manager.  The containers come preinstalled with
an OPAM environment, and are particularly suitable for use with continuous integration
systems such as [Travis CI](https://travis-ci.org).  All the containers are hosted
on the [Docker Hub ocaml/opam2](http://hub.docker.com/r/ocaml/opam2) repository.

Using it is as simple as:

```
docker pull %s
docker run -it %s bash
```

This will get you an interactive development environment (including on [Docker for Mac](https://www.docker.com/docker-mac)).  You can grab a specific OS distribution and test out external dependencies as well:

```
docker run %s:ubuntu opam depext -i cohttp-lwt-unix tls
```

There are a number of different variants available that are regularly rebuilt on the ocaml.org infrastructure and pushed to the [Docker Hub](http://hub.docker.com/r/ocaml/opam2).


Using The Defaults
==================

The `%s` Docker remote has a default `latest` tag that provides the %s Linux distribution with the latest release of the OCaml compiler (%s).
The [opam-depext](https://github.com/ocaml/opam-depext) plugin can be used to install external system libraries in a distro-portable way.

The default user is `opam` in the `/home/opam` directory, with a copy of the [opam-repository](https://github.com/ocaml/opam-repository)
checked out in `/home/opam/opam-repository`.  You can supply your own source code by volume mounting it anywhere in the container,
but bear in mind that it should be owned by the `opam` user (uid `1000` in all distributions).


Selecting a Specific Compiler
=============================

The default container comes with the latest compiler activated, but also a number of other switches for older revisions of OCaml.  You can
switch to these to test compatibility in CI by iterating through older revisions.

For example:

```
$ docker run %s opam switch
    switch  compiler                    description
    4.02    ocaml-base-compiler.4.02.3  4.02
    4.03    ocaml-base-compiler.4.03.0  4.03
    4.04    ocaml-base-compiler.4.04.2  4.04
    4.05    ocaml-base-compiler.4.05.0  4.05
->  4.06    ocaml-base-compiler.4.06.1  4.06
```

Note that the name of the switch drops the minor patch release (e.g. `4.06` _vs_ `4.06.1`), since you should always be using the latest patch revision of the compiler.

Accessing Compiler Variants
===========================

Modern versions of OCaml also feature a number of variants, such as the experimental flambda inliner or [AFL fuzzing](http://lcamtuf.coredump.cx/afl/) support.  These are also conveniently available using the `<VERSION>` tag. For example:

```
$ docker run %s:4.06 opam switch
    switch                      compiler                                     description
->  4.06                        ocaml-base-compiler.4.06.1                   4.06
    4.06+afl                    ocaml-variants.4.06.1+afl                    4.06+afl
    4.06+default-unsafe-string  ocaml-variants.4.06.1+default-unsafe-string  4.06+default-unsafe-string
    4.06+flambda                ocaml-variants.4.06.1+flambda                4.06+flambda
    4.06+force-safe-string      ocaml-variants.4.06.1+force-safe-string      4.06+force-safe-string
```

In this case, the `4.06` container has the latest patch release (4.06.1) activated by default, but the other variant compilers are available easily via `opam switch` without having to compile them yourself.  Using this more specific tag also helps you pin the version of OCaml that your CI system will be testing with, as the default `latest` tag will be regularly upgraded to keep up with upstream OCaml releases.


Selecting Linux Distributions
=============================

There are also tags available to select other Linux distributions, which is useful to validate and test the behaviour of your package in CI.

Distribution | Tag | Architectures | Command
------------ | --- | ------------- | -------
%s

The tags above are for the latest version of the distribution, and are upgraded to the latest upstream stable releases.  You can also select a specific version number in the tag name to obtain a particular OS release.  However, these will eventually time out once they are out of their support window, so try to use the version-free aliases described earlier unless you really know that you want a specific release.  When a specific release does time out, the container will be replaced by one that always displays an error message requesting you to upgrade your CI script.


Distribution | Tag | Architectures | Command
------------ | --- | ------------- | -------
%s


Multi-architecture Containers
=============================

The observant reader will notice that the distributions listed above have more than one architecture.  We are building an increasing number of packages on non-x86 containers, starting with ARM64 and soon to include PPC64.

Using the multiarch images is simple, as the correct one will be selected depending on your host architecture.  The images are built using [docker manifest](https://docs.docker.com/edge/engine/reference/commandline/manifest/).

Development Versions of the Compiler
====================================

You can also access development versions of the OCaml compiler (currently %s) that have not yet been released.  These are rebuilt around once a day, so you may lag a few commits behind the main master branch.  Since these are not intended to be long-term supported containers, you must reference the distribution and ocaml version explicitly in the tag, by using the form `distro-VERSION-ocaml-VERSION`.  For example:

```
$ docker run -it ocaml/opam2:debian-9-ocaml-4.07 opam switch
    switch              compiler                             description
->  4.07                ocaml-variants.4.07.0+trunk          4.07
    4.07+trunk+afl      ocaml-variants.4.07.0+trunk+afl      4.07+trunk+afl
    4.07+trunk+flambda  ocaml-variants.4.07.0+trunk+flambda  4.07+trunk+flambda
$ docker run -it ocaml/opam2:debian-9-ocaml-4.07 ocaml --version
The OCaml toplevel, version 4.07.0+dev6-2018-04-10
```

There are a large number of distribution and OCaml version combinations that are regularly built.  For the advanced user who needs a specific combination, the full current list can be found on the [Docker Hub](http://hub.docker.com/r/ocaml/opam2).  However, please try to use the shorter aliases rather than these explicit versions if you can, since then your builds will not error as the upstream versions advance.

Package Sandboxing
==================

The Docker containers install opam2's [Bubblewrap](https://github.com/projectatomic/bubblewrap) tool that is used for sandboxing builds.  However, due to the way that Linux sandboxing works, this may not work with all Docker installations since unprivileged containers cannot create new Linux namespaces on some installations.  Thus, sandboxing is disabled by default in the containers that have opam initialised.

If you can run containers with `docker run --privileged`, then you can enable opam sandboxing within the container by running `opam-sandbox-enable` within the container.  This will ensure that every package is restricted to only writing within `~/.opam` and is the recommended way of doing testing.

Questions and Feedback
======================

We are constantly improving and maintaining this infrastructure, so please get in touch with Anil Madhavapeddy `<anil@recoil.org>` if you have any questions or requests for improvement.  Note that until opam 2.0 is released, this infrastructure is considered to be in a beta stage and subject to change.

This is all possible thanks to generous infrastructure contributions from [Packet.net](https://www.packet.net), [IBM](http://ibm.com), [Azure](https://azure.microsoft.com/en-gb/) and [Rackspace](http://rackspace.com), as well as a dedicated machine cluster funded by [Jane Street](http://janestreet.com).  The Docker Hub also provides a huge amount of storage space for our containers.  We use hundreds of build agents running on [BuildKite](http://buildkite.com) in order to regularly generate the large volume of updates that this infrastructure needs, including the multiarch builds.

  |}
      prod_hub_id prod_hub_id prod_hub_id prod_hub_id
      (D.human_readable_string_of_distro D.master_distro)
      OV.(to_string Releases.latest)
      prod_hub_id prod_hub_id latest_distros active_distros
      dev_versions_of_ocaml
  in
  intro

let assoc_hashtbl l =
  let h = Hashtbl.create 7 in
  List.iter
    (fun (k, v) ->
      match Hashtbl.find h k with
      | _, a -> Hashtbl.replace h k (None, v :: a)
      | exception Not_found -> Hashtbl.add h k (None, [v]) )
    l ;
  h

let docker_build_and_push_cmds ~distro ~arch ~tag prefix =
  `A
    [ `String "docker login -u $DOCKER_HUB_USER -p $DOCKER_HUB_PASSWORD"
    ; `String
        (Fmt.strf
           "docker build --force-rm --rm --pull -t %s -f  %s-%s/Dockerfile.%s ."
           tag prefix arch distro)
    ; `String (Fmt.strf "docker push %s" tag)
    ; `String (Fmt.strf "docker rmi %s" tag) ]

let gen_multiarch ~staging_hub_id ~prod_hub_id h suffix name =
  Hashtbl.fold
    (fun f (tag, arches) acc ->
      let tags =
        List.map
          (fun arch ->
            Fmt.strf "%s:%s%s-linux-%s" staging_hub_id f suffix
              (OV.string_of_arch arch) )
          arches
      in
      let l = String.concat " " tags in
      let pulls =
        List.map (fun t -> `String (Fmt.strf "docker pull %s" t)) tags
      in
      let tag =
        match tag with None -> Fmt.strf "%s%s" f suffix | Some t -> t
      in
      let annotates =
        List.map2
          (fun tag arch ->
            let flags =
              match arch with
              | `Aarch32 -> "--arch arm --variant v7"
              | `Aarch64 -> "--arch arm64 --variant v8"
              | `X86_64 -> "--arch amd64"
              | `Ppc64le -> "--arch ppc64le"
            in
            `String
              (Fmt.strf "docker manifest annotate %s:%s%s %s %s" prod_hub_id f
                 suffix tag flags) )
          tags arches
      in
      let script =
        `A
          ( [`String "docker login -u $DOCKER_HUB_USER -p $DOCKER_HUB_PASSWORD"]
          @ pulls
          @ [ `String
                (Fmt.strf "docker manifest push -p %s:%s || true" prod_hub_id tag)
            ; `String
                (Fmt.strf "docker manifest create %s:%s %s" prod_hub_id tag l)
            ]
          @ annotates
          @ [ `String (Fmt.strf "docker manifest inspect %s:%s" prod_hub_id tag)
            ; `String
                (Fmt.strf "docker manifest push -p %s:%s" prod_hub_id tag) ] )
      in
      let cmds : Yaml.value =
        `O
          [ ("stage", `String (Fmt.strf "%s-multiarch" name))
          ; ("retry", `String "2")
          ; ("except", `A [`String "pushes"])
          ; ("tags", `A [`String "shell"; `String "amd64"])
          ; ("script", script) ]
      in
      let jobname = match tag.[0] with '0' .. '9' -> "v" ^ tag | _ -> tag in
      (jobname, cmds) :: acc )
    h []

let gen_ocaml ({staging_hub_id; prod_hub_id; results_dir; _} as opts) () =
  ignore (Bos.OS.Dir.create ~path:true results_dir) ;
  let ocaml_dockerfiles =
    List.map
      (fun arch ->
        let prefix = Fmt.strf "ocaml-%s" (OV.string_of_arch arch) in
        let results_dir = Fpath.(results_dir / prefix) in
        ignore (Bos.OS.Dir.create ~path:true results_dir) ;
        let all_compilers =
          D.active_distros arch
          |> List.map (O.all_ocaml_compilers prod_hub_id arch)
        in
        let each_compiler =
          D.active_tier1_distros arch
          |> List.map (O.separate_ocaml_compilers prod_hub_id arch)
          |> List.flatten
        in
        let dfiles = all_compilers @ each_compiler in
        ignore (G.generate_dockerfiles ~crunch:true results_dir dfiles) ;
        List.map (fun (f, _) -> (f, arch)) dfiles )
      arches
    |> List.flatten
  in
  let ocaml_arch_builds =
    List.map
      (fun (f, arch) ->
        let arch = OV.string_of_arch arch in
        let label = Fmt.strf "%s-linux-%s" f arch in
        let tag = Fmt.strf "%s:%s" staging_hub_id label in
        let cmds =
          `O
            [ ("stage", `String "ocaml-builds")
            ; ("retry", `String "2")
            ; ("except", `A [`String "pushes"])
            ; ("tags", `A [`String "shell"; `String arch])
            ; ( "script"
              , docker_build_and_push_cmds ~distro:f ~arch ~tag "ocaml" ) ]
        in
        (label, cmds) )
      ocaml_dockerfiles
  in
  let ocaml_dockerfiles_by_arch = assoc_hashtbl ocaml_dockerfiles in
  let ocaml_multiarch_dockerfiles =
    gen_multiarch ~staging_hub_id ~prod_hub_id ocaml_dockerfiles_by_arch ""
      "ocaml"
  in
  (* Generate aliases for OCaml releases and distros *)
  let distro_alias_multiarch =
    let distro_aliases = Hashtbl.create 7 in
    List.iter
      (fun ldistro ->
        let distro = D.resolve_alias ldistro in
        let f = D.tag_of_distro distro in
        let tag = D.tag_of_distro ldistro in
        let arches =
          try snd (Hashtbl.find ocaml_dockerfiles_by_arch f)
          with Not_found -> []
        in
        Hashtbl.add distro_aliases f (Some tag, arches) ;
        (* Add an alias for "latest" for Debian Stable too *)
        if ldistro = `Debian `Stable then
          Hashtbl.add distro_aliases f (Some "latest", arches) )
      D.latest_distros ;
    gen_multiarch ~staging_hub_id ~prod_hub_id distro_aliases "" "alias"
  in
  let ocaml_alias_multiarch =
    let ocaml_aliases = Hashtbl.create 7 in
    List.iter
      (fun ov ->
        let ov = OV.with_patch ov None in
        let distro = D.resolve_alias (`Debian `Stable) in
        let f =
          Fmt.strf "%s-ocaml-%s" (D.tag_of_distro distro) (OV.to_string ov)
        in
        let arches =
          try snd (Hashtbl.find ocaml_dockerfiles_by_arch f)
          with Not_found -> []
        in
        let tag = OV.to_string ov in
        Hashtbl.add ocaml_aliases f (Some tag, arches) )
      OV.Releases.recent ;
    gen_multiarch ~staging_hub_id ~prod_hub_id ocaml_aliases "" "alias"
  in
  let yml =
    `O
      ( ocaml_arch_builds @ ocaml_multiarch_dockerfiles @ distro_alias_multiarch
      @ ocaml_alias_multiarch )
    |> Yaml.to_string_exn ~len:256000
  in
  Bos.OS.File.write Fpath.(results_dir / "ocaml-builds.yml") yml
  >>= fun () -> Bos.OS.File.write Fpath.(results_dir / "README.md") (docs opts)

let gen_opam ({staging_hub_id; prod_hub_id; results_dir; _} as opts) () =
  ignore (Bos.OS.Dir.create ~path:true results_dir) ;
  let opam_dockerfiles =
    List.map
      (fun arch ->
        let prefix = Fmt.strf "opam-%s" (OV.string_of_arch arch) in
        let results_dir = Fpath.(results_dir / prefix) in
        ignore (Bos.OS.Dir.create ~path:true results_dir) ;
        let distros =
          List.filter
            (D.distro_supported_on arch OV.Releases.latest)
            (D.active_distros arch)
        in
        let dfiles = List.map O.gen_opam2_distro distros in
        ignore (G.generate_dockerfiles ~crunch:true results_dir dfiles) ;
        List.map (fun (f, _) -> (f, arch)) dfiles )
      arches
    |> List.flatten
  in
  let opam_arch_builds =
    List.map
      (fun (f, arch) ->
        let arch = OV.string_of_arch arch in
        let tag = Fmt.strf "%s:%s-opam-linux-%s" staging_hub_id f arch in
        let label = Fmt.strf "%s-opam-linux-%s" f arch in
        let cmds =
          `O
            [ ("stage", `String "opam-builds")
            ; ("retry", `String "2")
            ; ("except", `A [`String "pushes"])
            ; ("tags", `A [`String "shell"; `String arch])
            ; ("script", docker_build_and_push_cmds ~distro:f ~arch ~tag "opam")
            ]
        in
        (label, cmds) )
      opam_dockerfiles
  in
  let opam_dockerfiles_by_arch = assoc_hashtbl opam_dockerfiles in
  let opam_multiarch_dockerfiles =
    gen_multiarch ~staging_hub_id ~prod_hub_id opam_dockerfiles_by_arch "-opam"
      "opam"
  in
  let yml =
    `O (opam_arch_builds @ opam_multiarch_dockerfiles)
    |> Yaml.to_string_exn ~len:256000
  in
  Bos.OS.File.write Fpath.(results_dir / "opam-builds.yml") yml
  >>= fun () -> Bos.OS.File.write Fpath.(results_dir / "README.md") (docs opts)

let pkg_version pkg =
  let open Astring in
  match String.cut ~sep:"." pkg with
  | None -> Error (`Msg "invalid pkg")
  | Some (pkg, ver) -> Ok (pkg, ver)

open Cmdliner

let setup_logs = C.setup_logs ()

let fpath = Arg.conv ~docv:"PATH" (Fpath.of_string, Fpath.pp)

let arch =
  let doc = "CPU architecture to perform build on" in
  let term =
    Arg.enum [("amd64", `X86_64); ("arm64", `Aarch64); ("ppc64le", `Ppc64le)]
  in
  Arg.(value & opt term `X86_64 & info ["arch"] ~docv:"ARCH" ~doc)

let opam_repo_rev =
  let doc = "opam repo git rev" in
  let open Arg in
  required
  & opt (some string) None
  & info ["opam-repo-rev"] ~docv:"OPAM_REPO_REV" ~doc

let copts_t =
  let docs = Manpage.s_common_options in
  let staging_hub_id =
    let doc = "Docker Hub user/repo to push to for staging builds" in
    let open Arg in
    value
    & opt string "ocaml/opam2-staging"
    & info ["staging-hub-id"] ~docv:"STAGING_HUB_ID" ~doc ~docs
  in
  let prod_hub_id =
    let doc =
      "Docker Hub user/repo to push to for production multiarch builds"
    in
    let open Arg in
    value & opt string "ocaml/opam2"
    & info ["prod-hub-id"] ~docv:"PROD_HUB_ID" ~doc ~docs
  in
  let results_dir =
    let doc = "Directory in which to store bulk build results" in
    let open Arg in
    value
    & opt fpath (Fpath.v "_results")
    & info ["o"; "results-dir"] ~docv:"RESULTS_DIR" ~doc ~docs
  in
  Term.(const copts $ staging_hub_id $ prod_hub_id $ results_dir)

let buildv ov distro =
  Ocaml_version.of_string_exn ov
  |> fun ov ->
  let distro =
    match D.distro_of_tag distro with
    | None -> failwith "unknown distro"
    | Some distro -> distro
  in
  {ov; distro}

let build_t =
  let ocaml_version =
    let doc = "ocaml version to build" in
    let env = Arg.env_var "OCAML_VERSION" ~doc in
    let open Arg in
    value & opt string "4.06.1"
    & info ["ocaml-version"] ~docv:"OCAML_VERSION" ~env ~doc
  in
  let distro =
    let doc = "distro to build" in
    let env = Arg.env_var "DISTRO" ~doc in
    let open Arg in
    value & opt string "debian-9" & info ["distro"] ~env ~docv:"DISTRO" ~doc
  in
  Term.(const buildv $ ocaml_version $ distro)

let opam_cmd =
  let doc = "generate, build and push base opam container images" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Generate and build base $(b,opam) container images." ]
  in
  ( Term.(term_result (const gen_opam $ copts_t $ setup_logs))
  , Term.info "opam" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )

let ocaml_cmd =
  let doc = "generate, build and push base ocaml container images" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Generate and build base $(b,ocaml) container images." ]
  in
  ( Term.(term_result (const gen_ocaml $ copts_t $ setup_logs))
  , Term.info "ocaml" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )

let default_cmd =
  let doc = "build and push opam and OCaml multiarch container images" in
  let sdocs = Manpage.s_common_options in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
  , Term.info "obi-git" ~version:"%%VERSION%%" ~doc ~sdocs )

let cmds = [opam_cmd; ocaml_cmd]

let () = Term.(exit @@ eval_choice default_cmd cmds)
