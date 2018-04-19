module L = Dockerfile_linux
module D = Dockerfile_distro
module C = Dockerfile_cmd
module G = Dockerfile_gen
module O = Dockerfile_opam
module OV = Ocaml_version
open Bos
open Rresult
open R.Infix

type copts =
  { staging_hub_id: string
  ; prod_hub_id: string
  ; results_dir: Fpath.t }

let copts staging_hub_id prod_hub_id results_dir =
  { staging_hub_id
  ; prod_hub_id
  ; results_dir }


type build_t = {ov: Ocaml_version.t; distro: D.t}

let docs {prod_hub_id;_} =
  let distros ds =
    List.map (fun distro ->
      let name = D.human_readable_string_of_distro distro in
      let tag = D.tag_of_distro distro in
      let arches = String.concat " " (List.map OV.string_of_arch (D.distro_arches OV.Releases.latest distro)) in
      Fmt.strf "| %s | `%s` | %s | `docker run %s:%s`" name tag arches prod_hub_id tag
    ) ds |> String.concat "\n" in
  let latest_distros = distros D.latest_distros in
  let active_distros = distros (D.active_distros `X86_64) in
  let dev_versions_of_ocaml = String.concat " " (List.map OV.to_string OV.Releases.dev) in
  let intro =  Fmt.strf {|# OCaml Container Infrastructure

This repository contains a set of [Docker](http://docker.com) container definitions
for various combination of [OCaml](https://ocaml.org) and the
[OPAM](https://opam.ocaml.org) package manager.  The containers come preinstalled with
an OPAM environment, and are particularly suitable for use with continuous integration
systems such as [Travis CI](https://travis-ci.org).  All the containers are hosted
on the [Docker Hub ocaml/opam2](http://hub.docker.com/r/ocaml/opam2) repository.

**Note: this is still under development, so in the examples below you will need to use
`ocaml/opam2-staging` instead of `ocaml/opam2` until we finish testing.**

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
$ docker run -it ocaml/opam2-staging:debian-9-ocaml-4.07 opam switch
    switch              compiler                             description
->  4.07                ocaml-variants.4.07.0+trunk          4.07
    4.07+trunk+afl      ocaml-variants.4.07.0+trunk+afl      4.07+trunk+afl
    4.07+trunk+flambda  ocaml-variants.4.07.0+trunk+flambda  4.07+trunk+flambda
$ docker run -it ocaml/opam2-staging:debian-9-ocaml-4.07 ocaml --version
The OCaml toplevel, version 4.07.0+dev6-2018-04-10
```

There are a large number of distribution and OCaml version combinations that are regularly built.  For the advanced user who needs a specific combination, the full current list can be found on the [Docker Hub](http://hub.docker.com/r/ocaml/opam2).  However, please try to use the shorter aliases rather than these explicit versions if you can, since then your builds will not error as the upstream versions advance.

Questions and Feedback
======================

We are constantly improving and maintaining this infrastructure, so please get in touch with Anil Madhavapeddy `<anil@recoil.org>` if you have any questions or requests for improvement.  Note that until opam 2.0 is released, this infrastructure is considered to be in a beta stage and subject to change.

This is all possible thanks to generous infrastructure contributions from [Packet.net](https://www.packet.net), [IBM](http://ibm.com), [Azure](https://azure.microsoft.com/en-gb/) and [Rackspace](http://rackspace.com), as well as a dedicated machine cluster funded by [Jane Street](http://janestreet.com).  The Docker Hub also provides a huge amount of storage space for our containers.  We use hundreds of build agents running on [BuildKite](http://buildkite.com) in order to regularly generate the large volume of updates that this infrastructure needs, including the multiarch builds.

  |} prod_hub_id prod_hub_id prod_hub_id prod_hub_id (D.human_readable_string_of_distro D.master_distro) OV.(to_string Releases.latest) prod_hub_id prod_hub_id latest_distros active_distros dev_versions_of_ocaml in
  intro

let arches = [ `X86_64; `Aarch64 ]

let docker_login = "plugins", `O [ "docker-login#v1.0.0", `O [ "username", `String "avsm" ] ]
let concurrency num group =
  [ "concurrency", `String (string_of_int num);
  "concurrency_group", `String group ]

let docker_agents arch =
   "agents", `O [ "arch", `String "amd64";
                  "docker", `String "true";
                  "pusher", `String "true";
                  "os", `String "linux" ]

let bulk ({staging_hub_id; results_dir; _}) opam_repo_rev () =
  ignore (Bos.OS.Dir.create ~path:true results_dir);
  let distro = D.resolve_alias (`Debian `Stable) in
  let variant = None in
  let arch = `X86_64 in
  let ov = OV.(with_patch Releases.latest None |> fun ov -> with_variant ov variant) in
  let dfiles = 
    let open Dockerfile in
    O.bulk_build staging_hub_id distro ov opam_repo_rev @@
    copy ~src:["opam-ci-install"] ~dst:"/usr/bin/opam-ci-install" () @@
    run "sudo chmod a+x /usr/bin/opam-ci-install"
  in
  let tag = Fmt.strf "bulk-%s-%s-linux-%s-%s" (D.tag_of_distro distro) (OV.to_string ov |> String.map (function '+' -> '-' | x -> x)) (OV.string_of_arch arch) opam_repo_rev in
  let label = Fmt.strf "Bulk %s %s %s: %s" (D.tag_of_distro distro) (OV.to_string ov) (OV.string_of_arch arch) opam_repo_rev in
  let dir = Fpath.(results_dir / tag) in
  ignore (Bos.OS.Dir.create ~path:true dir);
  ignore(G.generate_dockerfiles ~crunch:false dir [ opam_repo_rev, dfiles] );
  let bulk_tmpl =
    let cmds = `A [
      `String (Fmt.strf "docker pull %s:%s" staging_hub_id tag);
      `String (Fmt.strf "mkdir -p %s/results" tag);
      `String (Fmt.strf "docker run --rm -v opam2-archive:/home/opam/.opam/download-cache %s:%s opam-ci-install __PKG__ > %s/results/__PKG__.txt" staging_hub_id tag tag);
      `String (Fmt.strf "buildkite-agent artifact upload %s/results/__PKG__.txt" tag)
    ] in
    let label = `String "__PKG__" in
    `O [ "steps", `A [ `O [ "commands", cmds; "label", label; docker_agents arch ] ] ] in
  ignore (Bos.OS.File.write Fpath.(dir / "template.yml") (Yaml.to_string_exn bulk_tmpl));
  let cmds =
    `A [
      `String (Fmt.strf "buildkite-agent artifact download '%s/*' ." tag);
      `String (Fmt.strf "buildkite-agent artifact download 'opam-ci-install' .");
      `String (Fmt.strf "docker build --rm --pull -t %s:%s -f %s/Dockerfile.%s ." staging_hub_id tag tag opam_repo_rev);
      `String (Fmt.strf "docker push %s:%s" staging_hub_id tag);
      `String (Fmt.strf "docker run %s:%s opam list --all-versions --installable -s > %s/pkgs.txt" staging_hub_id tag tag);
      `String (Fmt.strf "buildkite-agent artifact upload %s/pkgs.txt" tag);
      `String (Fmt.strf "cat %s/pkgs.txt | xargs -n 1 -I __NAME__ sh -c \"sed -e 's/__PKG__/__NAME__/g' < %s/template.yml > %s/build-__NAME__.yml\"" tag tag tag);
      `String (Fmt.strf "echo steps: > all.yml && cat %s/build-*.yml | grep -v ^steps >> all.yml" tag);
      `String (Fmt.strf "buildkite-agent pipeline upload all.yml" );
    ] in
  let p1_builds = `O ([ "command", cmds; "label", `String label; docker_agents arch; docker_login ]) in
  let gather_cmds = `A [
    `String (Fmt.strf "mkdir -p results-%s" tag);
    `String (Fmt.strf "buildkite-agent artifact download '%s/results/*' results-%s" tag tag);
    `String (Fmt.strf "tar -jcvf results-%s.tar.bz2 results-%s" tag tag);
    `String (Fmt.strf "echo %s > results-%s/arch" (OV.string_of_arch arch) tag);
    `String (Fmt.strf "echo %s > results-%s/ov" (OV.to_string ov) tag);
    `String (Fmt.strf "echo %s > results-%s/distro" (D.tag_of_distro distro) tag);
    `String (Fmt.strf "echo %s > results-%s/rev" opam_repo_rev tag);
    `String (Fmt.strf "buildkite-agent artifact upload results-%s.tar.bz2" tag);
  ] in
  let gather = [ `O (["command", gather_cmds; "label", `String "Gather Results"]) ] in
  let yml = `O [ "steps", `A ( p1_builds :: `String "wait" :: gather) ] in
  Bos.OS.File.write Fpath.(results_dir / "bulk.yml") (Yaml.to_string_exn ~len:128000 yml)

let gen ({staging_hub_id; results_dir; _} as opts) () =
  ignore (Bos.OS.Dir.create ~path:true results_dir);
  let p1 =
    List.map (fun arch ->
      let arch_s = OV.string_of_arch arch in
      let prefix = Fmt.strf "phase1-%s" arch_s in
      let results_dir = Fpath.(results_dir / prefix) in
      ignore (Bos.OS.Dir.create ~path:true results_dir);
      let distros = List.filter (D.distro_supported_on arch OV.Releases.latest) (D.active_distros arch) in
      let dfiles = List.map O.gen_opam2_distro distros in
      ignore (G.generate_dockerfiles ~crunch:true results_dir dfiles);
      List.map (fun (f,_) -> f,arch) dfiles
    ) arches |> List.flatten in
  let p2 = Hashtbl.create 9 in
  List.iter (fun (f,arch) ->
    match Hashtbl.find p2 f with
    | a -> Hashtbl.replace p2 f (arch :: a)
    | exception Not_found -> Hashtbl.add p2 f [arch]) p1;
  let p1_builds =
    List.map (fun (f,arch) ->
      let arch = OV.string_of_arch arch in
      let tag = Fmt.strf "%s:%s-opam-linux-%s" staging_hub_id f arch in
      let label = Fmt.strf ":linux: %s %s" f arch in
      let cmds = `A [
        `String (Fmt.strf "buildkite-agent artifact download phase1-%s/Dockerfile.%s ." arch f);
        `String (Fmt.strf "docker build --no-cache --rm --pull -t %s -f phase1-%s/Dockerfile.%s ." tag arch f);
        `String (Fmt.strf "docker push %s" tag)
      ] in
      `O ([ "command", cmds;
           "label", `String label;
           docker_agents arch;
           docker_login ])
      ) p1
  in
  let p2_march =
    Hashtbl.fold (fun f arches acc ->
      let tags = List.map (fun arch -> Fmt.strf "%s:%s-opam-linux-%s" staging_hub_id f (OV.string_of_arch arch)) arches in
      let l = String.concat " " tags in
      let pulls = List.map (fun t -> `String (Fmt.strf "docker pull %s" t)) tags in
      let annotates = List.map2 (fun tag arch -> `String (Fmt.strf "docker manifest annotate %s:%s-opam %s --arch %s" staging_hub_id f tag (OV.string_of_arch arch))) tags arches in
      let label = Fmt.strf ":docker: %s-opam" f in
      let cmds = `A (pulls @ [
        `String (Fmt.strf "docker manifest push -p %s:%s-opam || true" staging_hub_id f); 
        `String (Fmt.strf "docker manifest create %s:%s-opam %s" staging_hub_id f l);
      ] @ annotates @ [
        `String (Fmt.strf "docker manifest push -p %s:%s-opam" staging_hub_id f)
      ]) in
      `O ([ "command", cmds;
           "label", `String label;
           docker_agents "amd64";
           docker_login] @ (concurrency 5 "containers/ocaml")) :: acc) p2 [] in
  let p3 =
    List.map (fun arch ->
      let arch_s = OV.string_of_arch arch in
      let prefix = Fmt.strf "phase3-%s" arch_s in
      let results_dir = Fpath.(results_dir / prefix) in
      ignore (Bos.OS.Dir.create ~path:true results_dir);
      let all_compilers =
        D.active_distros arch |>
        List.map (O.all_ocaml_compilers staging_hub_id arch) in
      let each_compiler =
        D.active_tier1_distros arch |>
        List.map (O.separate_ocaml_compilers staging_hub_id arch) |> List.flatten in
      let dfiles = all_compilers @ each_compiler in
      ignore (G.generate_dockerfiles ~crunch:true results_dir dfiles);
      List.map (fun (f,_) -> f,arch) dfiles
    ) arches |> List.flatten in
  let p3_builds =
    List.map (fun (f,arch) ->
      let arch = OV.string_of_arch arch in
      let tag = Fmt.strf "%s:%s-linux-%s" staging_hub_id f arch in
      let label = Fmt.strf ":camel: %s %s" f arch in
      let cmds = `A [
        `String (Fmt.strf "buildkite-agent artifact download phase3-%s/Dockerfile.%s ." arch f);
        `String (Fmt.strf "docker build --no-cache --rm --pull -t %s -f phase3-%s/Dockerfile.%s ." tag arch f);
        `String (Fmt.strf "docker push %s" tag)
      ] in
      `O ([ "command", cmds; "label", `String label; docker_agents arch; docker_login ])
      ) p3
  in
  let p4 = Hashtbl.create 9 in
  List.iter (fun (f,arch) ->
    match Hashtbl.find p4 f with
    | a -> Hashtbl.replace p4 f (arch :: a)
    | exception Not_found -> Hashtbl.add p4 f [arch]) p3;
  let p3_march =
    Hashtbl.fold (fun f arches acc ->
      let l = String.concat " " (List.map (fun arch -> Fmt.strf "%s:%s-linux-%s" staging_hub_id f (OV.string_of_arch arch)) arches) in
      let label = Fmt.strf ":docker: %s" f in
      let cmds = `A [
        `String (Fmt.strf "docker manifest create -a %s:%s %s" staging_hub_id f l);
        `String (Fmt.strf "docker manifest inspect %s:%s" staging_hub_id f);
        `String (Fmt.strf "docker manifest push %s:%s" staging_hub_id f)
      ] in
      `O ([ "command", cmds; "label", `String label; docker_agents "amd64";
           docker_login] @ (concurrency 5 "containers/ocaml")) :: acc) p4 [] in
  let p4_march =
    List.fold_left (fun acc ldistro ->
      let distro = D.resolve_alias ldistro in
      let f = Fmt.strf "%s-ocaml" (D.tag_of_distro distro) in
      let arches = Hashtbl.find p4 f in
      let tags = List.map (fun arch -> Fmt.strf "%s:%s-linux-%s" staging_hub_id f (OV.string_of_arch arch)) arches in
      let l = String.concat " " tags in
      let pulls = List.map (fun t -> `String (Fmt.strf "docker pull %s" t)) tags in
      let tag = D.tag_of_distro ldistro in
      let label = Fmt.strf ":docker: %s" tag in
      let cmds = `A (pulls @ [
        `String (Fmt.strf "docker manifest create -a %s:%s %s" staging_hub_id tag l);
        `String (Fmt.strf "docker manifest inspect %s:%s" staging_hub_id tag);
        `String (Fmt.strf "docker manifest push %s:%s" staging_hub_id tag)
      ]) in
      `O ([ "command", cmds; "label", `String label; docker_agents "amd64";
           docker_login] @ (concurrency 5 "containers/ocaml")) :: acc)
    [] D.latest_distros in
  let p5_march =
    List.fold_left (fun acc ov ->
      let ov = OV.with_patch ov None in
      let distro = D.resolve_alias (`Debian `Stable) in
      let f = Fmt.strf "%s-ocaml-%s" (D.tag_of_distro distro) (OV.to_string ov) in
      let arches = Hashtbl.find p4 f in
      let l = String.concat " " (List.map (fun arch -> Fmt.strf "%s:%s-linux-%s" staging_hub_id f (OV.string_of_arch arch)) arches) in
      let tag = Fmt.strf "%s" (OV.to_string ov) in
      let label = Fmt.strf ":docker: %s" tag in
      let cmds = `A [
        `String (Fmt.strf "docker manifest create -a %s:%s %s" staging_hub_id tag l);
        `String (Fmt.strf "docker manifest push %s:%s" staging_hub_id tag)
      ] in
      `O ([ "command", cmds; "label", `String label; docker_agents "amd64";
           docker_login] @ (concurrency 5 "containers/ocaml")) :: acc)
     [] OV.Releases.recent in
  let p6_march =
    let distro = D.resolve_alias (`Debian `Stable) in
    let f = Fmt.strf "%s-ocaml" (D.tag_of_distro distro) in
    let arches = Hashtbl.find p4 f in
    let l = String.concat " " (List.map (fun arch -> Fmt.strf "%s:%s-linux-%s" staging_hub_id f (OV.string_of_arch arch)) arches) in
    let label = Fmt.strf ":docker: latest" in
    let tag = "latest" in
    let cmds = `A [
        `String (Fmt.strf "docker manifest create -a %s:%s %s" staging_hub_id tag l);
        `String (Fmt.strf "docker manifest push %s:%s" staging_hub_id tag)
    ] in
    [`O ([ "command", cmds; "label", `String label; docker_agents "amd64";
           docker_login] @ (concurrency 5 "containers/ocaml")) ] in
 
  let wait = [`String "wait"] in
  let yml = `O [ "steps", `A (p1_builds @ wait @ p2_march @ wait @ p3_builds @ wait @ p3_march @ p4_march @ p5_march @ p6_march) ] in
  Bos.OS.File.write Fpath.(results_dir / "phase1.yml") (Yaml.to_string_exn ~len:128000 yml) >>= fun () ->
  Bos.OS.File.write Fpath.(results_dir / "README.md") (docs opts)

open Cmdliner

let setup_logs = C.setup_logs ()

let fpath = Arg.conv ~docv:"PATH" (Fpath.of_string, Fpath.pp)

let arch =
  let doc = "CPU architecture to perform build on" in
  let term = Arg.enum [("x86_64", `X86_64); ("aarch64", `Aarch64)] in
  Arg.(value & opt term `X86_64 & info ["arch"] ~docv:"ARCH" ~doc)

let opam_repo_rev =
  let doc = "opam repo git rev" in
  let open Arg in
  required & opt (some string) None
  & info ["opam-repo-rev"] ~docv:"OPAM_REPO_REV" ~doc

let copts_t =
  let docs = Manpage.s_common_options in
  let staging_hub_id =
    let doc = "Docker Hub user/repo to push to for staging builds" in
    let open Arg in
    value & opt string "ocaml/opam2-staging"
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
    value & opt fpath (Fpath.v "_results")
    & info ["o"; "results-dir"] ~docv:"RESULTS_DIR" ~doc ~docs
  in
  let open Term in
  const copts $ staging_hub_id $ prod_hub_id $ results_dir

let bulk_build =
  let doc = "perform a bulk build of the opam repository" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Perform a bulk build of the opam packages" ]
  in
  ( Term.(term_result (const bulk $ copts_t $ opam_repo_rev $ setup_logs))
  , Term.info "bulk" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )



let gen_cmd =
  let doc = "generate, build and push base opam container images" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Generate and build base $(b,opam) container images." ]
  in
  ( Term.(term_result (const gen $ copts_t $ setup_logs))
  , Term.info "gen" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )

let default_cmd =
  let doc = "build and push opam and OCaml multiarch container images" in
  let sdocs = Manpage.s_common_options in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
  , Term.info "obi-buildkite" ~version:"v1.0.0" ~doc ~sdocs )


let cmds = [ gen_cmd; bulk_build ]

let () = Term.(exit @@ eval_choice default_cmd cmds)
