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

let arches = [ `X86_64; `Aarch64 ]

let gen {staging_hub_id; results_dir; _} () =
  ignore (Bos.OS.Dir.create ~path:true results_dir);
  let p1 =
    List.map (fun arch ->
      let arch_s = OV.string_of_arch arch in
      let prefix = Fmt.strf "phase1-%s" arch_s in
      let results_dir = Fpath.(results_dir / prefix) in
      ignore (Bos.OS.Dir.create ~path:true results_dir);
      let tag s = Fmt.strf "%s:%s-opam-linux-%s" staging_hub_id s arch_s in
      let distros = List.filter (D.distro_supported_on arch OV.Releases.latest) (D.active_distros arch) in
      let dfiles = List.map O.gen_opam2_distro distros in
      ignore (G.generate_dockerfiles ~crunch:true results_dir dfiles);
      List.map (fun (f,_) -> f,arch) dfiles
    ) arches |> List.flatten in
  let yml =
      `O [ "steps",
        `A (
          List.map (fun (f,arch) ->
            let arch = OV.string_of_arch arch in
            let tag = Fmt.strf "%s:%s-opam-linux-%s" staging_hub_id f arch in
            let label = Fmt.strf "%s %s" f arch in
            let cmds = `A [
              `String (Fmt.strf "buildkite-agent artifact download phase1-%s/Dockerfile.%s ." arch f);
              `String (Fmt.strf "docker build --rm --no-cache -t %s -f phase1-%s/Dockerfile.%s ." tag arch f);
              `String (Fmt.strf "docker push %s" tag)
             ] in
            `O [ "command", cmds;
                 "label", `String label;
                 "agents", `O [ "arch", `String arch ];
                 "plugins", `O [ "docker-login#v1.0.0", `O [ "username", `String "avsm" ] ];
               ]
          ) p1
         )
      ]
    in
  Bos.OS.File.write Fpath.(results_dir / "phase1.yml") (Yaml.to_string_exn yml)

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
  let results_dir =
    let doc = "Directory in which to store bulk build results" in
    let open Arg in
    value & opt fpath (Fpath.v "_results")
    & info ["o"; "results-dir"] ~docv:"RESULTS_DIR" ~doc ~docs
  in
  let open Term in
  const copts $ staging_hub_id $ prod_hub_id $ results_dir


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
  , Term.info "obi-docker" ~version:"v1.0.0" ~doc ~sdocs )


let cmds =
  [ gen_cmd
  ]

let () = Term.(exit @@ eval_choice default_cmd cmds)
