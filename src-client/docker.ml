(* generate ocaml docker containers *)
module L = Dockerfile_linux
module D = Dockerfile_distro
module C = Dockerfile_cmd
module G = Dockerfile_gen
module O = Dockerfile_opam
module OV = Ocaml_version
open Bos
open Rresult
open R.Infix

let bulk_results_dir ~opam_repo_rev ~arch ~ov ~distro logs_dir =
  D.tag_of_distro distro
  |> fun distro ->
  OV.string_of_arch arch
  |> fun arch ->
  OV.to_string ov
  |> fun ov -> Fpath.(logs_dir / opam_repo_rev / arch / distro / ov)


let if_opt opt fn = if opt then fn () else Ok ()

let if_opt_m def opt fn = opt >>= function true -> fn () | false -> Ok def

type copts =
  { staging_hub_id: string
  ; prod_hub_id: string
  ; push: bool
  ; cache: bool
  ; build: bool
  ; arch: [`X86_64 | `Aarch64]
  ; build_dir: Fpath.t
  ; logs_dir: Fpath.t
  ; results_dir: Fpath.t }

let copts staging_hub_id prod_hub_id push cache build arch build_dir logs_dir
    results_dir =
  { staging_hub_id
  ; prod_hub_id
  ; push
  ; cache
  ; build
  ; arch
  ; build_dir
  ; logs_dir
  ; results_dir }


type build_t = {ov: Ocaml_version.t; distro: D.t}

module Phases = struct
  let setup_log_dirs ~prefix build_dir logs_dir fn =
    Fpath.(build_dir / prefix)
    |> fun build_dir ->
    Fpath.(logs_dir / prefix)
    |> fun logs_dir ->
    Bos.OS.Dir.create ~path:true build_dir
    >>= fun _ ->
    Bos.OS.Dir.create ~path:true logs_dir >>= fun _ -> fn build_dir logs_dir


  (* Generate base opam binaries for all distros *)
  let phase1 {cache; push; build; arch; staging_hub_id; build_dir; logs_dir} () =
    let arch_s = OV.string_of_arch arch in
    let prefix = Fmt.strf "phase1-%s" arch_s in
    setup_log_dirs ~prefix build_dir logs_dir
    @@ fun build_dir logs_dir ->
    let tag = Fmt.strf "%s:{}-opam-linux-%s" staging_hub_id arch_s in
    List.filter (D.distro_supported_on arch) D.active_distros
    |> List.map O.gen_opam2_distro
    |> fun ds ->
    G.generate_dockerfiles ~crunch:true build_dir ds
    >>= fun () ->
    if_opt build
    @@ fun () ->
    let dockerfile = Fpath.(build_dir / "Dockerfile.{}") in
    let cmd = C.Docker.build_cmd ~cache ~dockerfile ~tag (Fpath.v ".") in
    let args = List.map fst ds in
    C.Parallel.run ~retries:1 logs_dir "01-build" cmd args
    >>= fun _ ->
    if_opt push
    @@ fun () ->
    let cmd = C.Docker.push_cmd tag in
    C.Parallel.run ~retries:1 logs_dir "02-push" cmd args >>= fun _ -> Ok ()


  (* Push multiarch images to the Hub for base opam binaries *)
  let phase2 {prod_hub_id; staging_hub_id; push; build_dir; logs_dir} () =
    setup_log_dirs ~prefix:"phase2" build_dir logs_dir
    @@ fun build_dir logs_dir ->
    let yaml_file tag = Fpath.(build_dir / (tag ^ ".yml")) in
    let yamls =
      List.map
        (fun distro ->
          let tag = D.tag_of_distro distro in
          let target = Fmt.strf "%s:%s-opam" prod_hub_id tag in
          let platforms =
            D.distro_arches distro
            |> List.map (fun arch ->
                   let arch = OV.string_of_arch arch in
                   let image =
                     Fmt.strf "%s:%s-opam-linux-%s" staging_hub_id tag arch
                   in
                   (image, arch) )
          in
          O.multiarch_manifest ~target ~platforms |> fun m -> (tag, m))
        D.active_distros
    in
    C.iter (fun (t, m) -> Bos.OS.File.write (yaml_file t) m) yamls
    >>= fun () ->
    if_opt push
    @@ fun () ->
    let cmd = C.Docker.manifest_push_file (yaml_file "{}") in
    let args = List.map (fun (t, _) -> t) yamls in
    C.Parallel.run ~retries:1 logs_dir "01-manifest" cmd args
    >>= fun _ -> Ok ()


  (* Generate an opam archive suitable for pointing local builds at *)
  let phase3_archive
      {cache; push; build; staging_hub_id; prod_hub_id; build_dir; logs_dir} () =
    setup_log_dirs ~prefix:"phase3-archive" build_dir logs_dir
    @@ fun build_dir logs_dir ->
    G.generate_dockerfile ~crunch:true build_dir (O.opam2_mirror prod_hub_id)
    >>= fun () ->
    if_opt build
    @@ fun () ->
    let dockerfile = Fpath.(build_dir / "Dockerfile") in
    let cmd = C.Docker.build_cmd ~cache ~dockerfile ~tag:"{}" (Fpath.v ".") in
    let args = [Fmt.strf "%s:%s" staging_hub_id "opam2-archive"] in
    C.Parallel.run ~retries:1 logs_dir "01-build" cmd args
    >>= fun _ ->
    if_opt push
    @@ fun () ->
    let cmd = C.Docker.push_cmd "{}" in
    C.Parallel.run ~retries:1 logs_dir "02-push" cmd args >>= fun _ -> Ok ()


  let phase3_ocaml
      { cache
      ; push
      ; build
      ; arch
      ; staging_hub_id
      ; prod_hub_id
      ; build_dir
      ; logs_dir } () =
    let arch_s = OV.string_of_arch arch in
    let prefix = Fmt.strf "phase3-ocaml-%s" arch_s in
    setup_log_dirs ~prefix build_dir logs_dir
    @@ fun build_dir logs_dir ->
    let all_compilers =
      List.filter (D.distro_supported_on arch) D.active_distros
      |> List.map (O.all_ocaml_compilers prod_hub_id arch)
    in
    let each_compiler =
      List.filter (D.distro_supported_on arch) D.active_tier1_distros
      |> List.map (O.separate_ocaml_compilers prod_hub_id arch) |> List.flatten
    in
    let dfiles = all_compilers @ each_compiler in
    G.generate_dockerfiles ~crunch:true build_dir dfiles
    >>= fun () ->
    if_opt build
    @@ fun () ->
    let dockerfile = Fpath.(build_dir / "Dockerfile.{}") in
    let tag = Fmt.strf "%s:{}-linux-%s" staging_hub_id arch_s in
    let cmd = C.Docker.build_cmd ~cache ~dockerfile ~tag (Fpath.v ".") in
    let args = List.map fst dfiles in
    C.Parallel.run ~jobs:10 ~delay:5.0 ~retries:1 logs_dir "01-build" cmd args
    >>= fun _ ->
    if_opt push
    @@ fun () ->
    let cmd = C.Docker.push_cmd tag in
    C.Parallel.run ~retries:1 logs_dir "02-push" cmd args >>= fun _ -> Ok ()


  (* Push multiarch images to the Hub for ocaml binaries *)
  let phase4 {staging_hub_id; prod_hub_id; push; build_dir; logs_dir} () =
    setup_log_dirs ~prefix:"phase4" build_dir logs_dir
    @@ fun build_dir logs_dir ->
    let yaml_file tag = Fpath.(build_dir / (tag ^ ".yml")) in
    let yamls =
      List.map
        (fun distro ->
          let tag = D.tag_of_distro distro in
          let mega_ocaml =
            let target = Fmt.strf "%s:%s-ocaml" prod_hub_id tag in
            let platforms =
              D.distro_arches distro
              |> List.map (fun arch ->
                     let arch = OV.string_of_arch arch in
                     let image =
                       Fmt.strf "%s:%s-ocaml-linux-%s" staging_hub_id tag arch
                     in
                     (image, arch) )
            in
            let tag = Fmt.strf "%s-ocaml" tag in
            O.multiarch_manifest ~target ~platforms |> fun m -> (tag, m)
          in
          let each_ocaml =
            List.map
              (fun ov ->
                let target =
                  Fmt.strf "%s:%s-ocaml-%a" prod_hub_id tag OV.pp ov
                in
                let platforms =
                  D.distro_arches distro
                  |> List.filter (fun a -> OV.(Has.arch a ov))
                  |> List.map (fun arch ->
                         let arch = OV.string_of_arch arch in
                         let image =
                           Fmt.strf "%s:%s-ocaml-%a-linux-%s" staging_hub_id
                             tag OV.pp ov arch
                         in
                         (image, arch) )
                in
                let tag = Fmt.strf "%s-ocaml-%a" tag OV.pp ov in
                O.multiarch_manifest ~target ~platforms |> fun m -> (tag, m))
              OV.Releases.recent
          in
          mega_ocaml :: each_ocaml)
        D.active_distros
      |> List.flatten
    in
    C.iter (fun (t, m) -> Bos.OS.File.write (yaml_file t) m) yamls
    >>= fun () ->
    if_opt push
    @@ fun () ->
    let cmd = C.Docker.manifest_push_file (yaml_file "{}") in
    let args = List.map (fun (t, _) -> t) yamls in
    C.Parallel.run ~delay:0.1 ~retries:1 logs_dir "01-manifest" cmd args
    >>= fun _ -> Ok ()


  let phase5_prefix ~distro ~ov ~arch ~opam_repo_rev =
    Fmt.strf "base-linux-%s-%s-%s-%s" (D.tag_of_distro distro)
      (OV.to_string ~sep:'-' ov) (OV.string_of_arch arch) opam_repo_rev


  let phase5_tag ~staging_hub_id ~distro ~ov ~arch ~opam_repo_rev =
    Fmt.strf "%s:%s" staging_hub_id
      (phase5_prefix ~distro ~ov ~arch ~opam_repo_rev)


  (* Setup a bulk build image *)
  let phase5
      { arch
      ; cache
      ; staging_hub_id
      ; prod_hub_id
      ; build
      ; push
      ; build_dir
      ; logs_dir
      ; results_dir } {distro; ov} opam_repo_rev () =
    let prefix = phase5_prefix ~distro ~ov ~arch ~opam_repo_rev in
    setup_log_dirs ~prefix build_dir logs_dir
    @@ fun build_dir logs_dir ->
    let dfiles = O.bulk_build prod_hub_id distro ov opam_repo_rev in
    G.generate_dockerfiles ~crunch:false build_dir dfiles
    >>= fun () ->
    if_opt build
    @@ fun () ->
    let dockerfile = Fpath.(build_dir / "Dockerfile.{}") in
    let setup_tag =
      phase5_tag ~staging_hub_id ~distro ~ov ~arch ~opam_repo_rev
    in
    let cmd =
      C.Docker.build_cmd ~cache ~dockerfile ~tag:setup_tag (Fpath.v ".")
    in
    let args = List.map fst dfiles in
    C.Parallel.run ~retries:1 logs_dir "01-build" cmd args
    >>= fun _ ->
    let tag = phase5_tag ~staging_hub_id ~distro ~ov ~arch ~opam_repo_rev in
    OS.Cmd.(run Cmd.(C.Docker.bin % "tag" % setup_tag % tag))
    >>= fun () ->
    let opam_cmd =
      Cmd.of_list ["opam"; "list"; "--installable"; "--all-versions"; "-s"]
    in
    OS.Cmd.(run_out (C.Docker.run_cmd tag opam_cmd) |> to_string)
    >>= fun pkg_list ->
    let res_dir =
      bulk_results_dir ~opam_repo_rev ~arch ~ov ~distro results_dir
    in
    OS.Dir.create res_dir
    >>= fun _ ->
    OS.File.write Fpath.(res_dir / "pkgs.txt") pkg_list
    >>= fun () ->
    if_opt push
    @@ fun () ->
    let cmd = C.Docker.push_cmd "{}" in
    C.Parallel.run ~retries:1 logs_dir "02-push" cmd [tag] >>= fun _ -> Ok ()


  let phase5_setup {staging_hub_id} () =
    Cmd.(C.Docker.volume_cmd % "rm" % "-f" % "opam2-archive") |> OS.Cmd.run
    >>= fun () ->
    (* TODO docker pull archive *)
    C.Docker.run_cmd
      ~mounts:[("opam2-archive", "/home/opam/opam-repository/cache")]
      (Fmt.strf "%s:opam2-archive" staging_hub_id)
      (Cmd.v "true")
    |> OS.Cmd.run


  let phase5_build
      { arch
      ; cache
      ; staging_hub_id
      ; prod_hub_id
      ; build
      ; build_dir
      ; logs_dir
      ; results_dir } {distro; ov} pkg opam_repo_rev () =
    let prefix = phase5_prefix ~distro ~ov ~arch ~opam_repo_rev in
    setup_log_dirs ~prefix build_dir logs_dir
    @@ fun build_dir logs_dir ->
    let res_dir =
      bulk_results_dir ~opam_repo_rev ~arch ~ov ~distro results_dir
    in
    let tag = phase5_tag ~staging_hub_id ~distro ~ov ~arch ~opam_repo_rev in
    C.Docker.run_cmd
      ~volumes:[("opam2-archive", "/home/opam/.opam/download-cache")] tag
      Cmd.(v "opam" % "depext" % "-i" % pkg)
    |> C.run_log res_dir pkg


  let phase5_cluster {arch; build_dir; logs_dir; results_dir} {distro; ov}
      hosts opam_repo_rev () =
    (* TODO pass through arch in prefix and cmdline *)
    let opts =
      [ "--results-dir"
      ; Fpath.to_string results_dir
      ; "--distro"
      ; D.tag_of_distro distro
      ; "--ocaml-version"
      ; OV.to_string ov
      ; "--opam-repo-rev"
      ; opam_repo_rev ]
    in
    let prefix = phase5_prefix ~distro ~ov ~arch ~opam_repo_rev in
    setup_log_dirs ~prefix build_dir logs_dir
    @@ fun build_dir logs_dir ->
    let res_dir =
      bulk_results_dir ~opam_repo_rev ~arch ~ov ~distro results_dir
    in
    Bos.OS.File.read_lines Fpath.(res_dir / "pkgs.txt")
    >>= fun pkgs ->
    Random.self_init ();
    List.map (fun a -> (Random.bits ()), a) pkgs |> List.sort compare |> List.map snd |> fun pkgs ->
    let mode = `Remote (`Controlmaster, hosts) in
    let cmd =
      Cmd.(v "./obi-docker" % "phase5-build" %% of_list opts % "-vv" % "{}")
    in
    C.Parallel.run ~mode ~retries:1 logs_dir "03-cluster" cmd pkgs
    >>= fun _ -> Ok ()

end

open Cmdliner

let setup_logs = C.setup_logs ()

let fpath = Arg.conv ~docv:"PATH" (Fpath.of_string, Fpath.pp)

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
  let push =
    let doc = "Push result of builds to Docker Hub" in
    Arg.(value & opt bool true & info ["push"] ~docv:"PUSH" ~doc ~docs)
  in
  let cache =
    let doc =
      "Use Docker caching (normally only activate for development use)"
    in
    Arg.(value & opt bool false & info ["cache"] ~docv:"CACHE" ~doc ~docs)
  in
  let build =
    let doc =
      "Build the results (normally only disable for development use)"
    in
    Arg.(value & opt bool true & info ["build"] ~docv:"BUILD" ~doc ~docs)
  in
  let arch =
    let doc = "CPU architecture to perform build on" in
    let term = Arg.enum [("x86_64", `X86_64); ("aarch64", `Aarch64)] in
    Arg.(value & opt term `X86_64 & info ["arch"] ~docv:"ARCH" ~doc ~docs)
  in
  let build_dir =
    let doc = "Directory in which to store build artefacts" in
    let open Arg in
    value & opt fpath (Fpath.v "_obj")
    & info ["b"; "build-dir"] ~docv:"BUILD_DIR" ~doc ~docs
  in
  let logs_dir =
    let doc = "Directory in which to store logs" in
    let open Arg in
    value & opt fpath (Fpath.v "_logs")
    & info ["l"; "logs-dir"] ~docv:"LOG_DIR" ~doc ~docs
  in
  let results_dir =
    let doc = "Directory in which to store bulk build results" in
    let open Arg in
    value & opt fpath (Fpath.v "_results")
    & info ["o"; "results-dir"] ~docv:"RESULTS_DIR" ~doc ~docs
  in
  let open Term in
  const copts $ staging_hub_id $ prod_hub_id $ push $ cache $ build $ arch
  $ build_dir $ logs_dir $ results_dir


let phase1_cmd =
  let doc = "generate, build and push base opam container images" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Generate and build base $(b,opam) container images." ]
  in
  ( Term.(term_result (const Phases.phase1 $ copts_t $ setup_logs))
  , Term.info "phase1" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )


let phase2_cmd =
  let doc = "combine opam container images into multiarch versions" in
  let exits = Term.default_exits in
  ( Term.(term_result (const Phases.phase2 $ copts_t $ setup_logs))
  , Term.info "phase2" ~doc ~exits )


let phase3_archive_cmd =
  let doc = "generate a distribution archive mirror" in
  let exits = Term.default_exits in
  ( Term.(term_result (const Phases.phase3_archive $ copts_t $ setup_logs))
  , Term.info "phase3-cache" ~doc ~exits )


let phase3_ocaml_cmd =
  let doc = "generate a matrix of ocaml compilers" in
  let exits = Term.default_exits in
  ( Term.(term_result (const Phases.phase3_ocaml $ copts_t $ setup_logs))
  , Term.info "phase3-ocaml" ~doc ~exits )


let phase4_cmd =
  let doc = "combine ocaml container images into multiarch versions" in
  let exits = Term.default_exits in
  ( Term.(term_result (const Phases.phase4 $ copts_t $ setup_logs))
  , Term.info "phase4" ~doc ~exits )


let ssh_hosts =
  let doc = "cluster hosts to ssh to" in
  Arg.(value & opt (list string) [] & info ["hosts"] ~docv:"PUSH" ~doc)


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
    let open Arg in
    value & opt string "4.05.0"
    & info ["ocaml-version"] ~docv:"OCAML_VERSION" ~doc
  in
  let distro =
    let doc = "distro to build" in
    Arg.(value & opt string "alpine-3.6" & info ["distro"] ~docv:"DISTRO" ~doc)
  in
  Term.(const buildv $ ocaml_version $ distro)


let opam_repo_rev_t =
  let doc = "opam repo git rev" in
  let open Arg in
  required & opt (some string) None
  & info ["opam-repo-rev"] ~docv:"OPAM_REPO_REV" ~doc


let phase5_cmd =
  let doc =
    "create a bulk build base image and generate a package list for it"
  in
  let exits = Term.default_exits in
  ( Term.(term_result (const Phases.phase5 $ copts_t $ build_t $ opam_repo_rev_t $ setup_logs))
  , Term.info "phase5" ~doc ~exits )


let phase5_setup =
  let doc = "setup cluster hosts for a bulk build" in
  let exits = Term.default_exits in
  ( Term.(term_result (const Phases.phase5_setup $ copts_t $ setup_logs))
  , Term.info "phase5-setup" ~doc ~exits )


let phase5_build =
  let doc = "build one package in a bulk build" in
  let exits = Term.default_exits in
  let pkg =
    let doc = "Package to build" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)
  in
  ( (let open Term in
    term_result
      ( const Phases.phase5_build $ copts_t $ build_t $ pkg $ opam_repo_rev_t
      $ setup_logs ))
  , Term.info "phase5-build" ~doc ~exits )


let phase5_cluster =
  let doc = "run cluster build" in
  let exits = Term.default_exits in
  ( (let open Term in
    term_result
      ( const Phases.phase5_cluster $ copts_t $ build_t $ ssh_hosts
      $ opam_repo_rev_t $ setup_logs ))
  , Term.info "phase5-cluster" ~doc ~exits )


let default_cmd =
  let doc = "build and push opam and OCaml multiarch container images" in
  let sdocs = Manpage.s_common_options in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
  , Term.info "obi-docker" ~version:"v1.0.0" ~doc ~sdocs )


let cmds =
  [ phase1_cmd
  ; phase2_cmd
  ; phase3_archive_cmd
  ; phase3_ocaml_cmd
  ; phase4_cmd
  ; phase5_cmd
  ; phase5_build
  ; phase5_setup
  ; phase5_cluster ]


let () = Term.(exit @@ eval_choice default_cmd cmds)
