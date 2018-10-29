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

type build_t = {ov: Ocaml_version.t; distro: D.t}

module Rules = struct
  let base_dockerfile ~ov ~distro ~arch ~rev : string =
    let t = D.tag_of_distro distro in
    let f = Fmt.strf "linux-%s-%s-%s" t (OV.string_of_arch arch) rev in
    let ov = OV.(with_patch ov None |> to_string) in
    Fmt.strf
      {|
(rule (targets Dockerfile.%s) (action (with-stdout-to %%{targets} (
    progn
      (echo "FROM ocaml/opam2:%s\n")
      (echo "WORKDIR /home/opam/opam-repository\n")
      (echo "RUN git pull origin master\n")
      (echo "RUN git checkout %s\n")
      (echo "RUN opam update -uy\n")
      (echo "RUN opam switch %s\n")
      (echo "COPY obi-ci-install /usr/bin/obi-ci-install\n")
)))) |}
      f t rev ov

  let base_build ~ov ~distro ~arch ~rev : string =
    let t = D.tag_of_distro distro in
    let f = Fmt.strf "linux-%s-%s-%s" t (OV.string_of_arch arch) rev in
    Fmt.strf
      {|
(rule (targets %s.log) (deps Dockerfile.%s obi-ci-install)
  (action (with-stdout-to %%{targets} (run docker build -f Dockerfile.%s -t %s --rm --force-rm .)))) |}
      f f f f

  let build_one ~ov ~distro ~arch ~rev package : string =
    let t = D.tag_of_distro distro in
    let base = Fmt.strf "linux-%s-%s-%s" t (OV.string_of_arch arch) rev in
    let f =
      Fmt.strf "linux-%s-%s-%s-%s" (D.tag_of_distro distro)
        (OV.string_of_arch arch) package rev
    in
    Fmt.strf
      {|
(rule (targets %s.json) (deps %s.tar) (action (with-stdout-to %%{targets} (run tar -xOf %%{deps} %s.json))))
(rule (targets %s.txt) (deps %s.tar) (action (with-stdout-to %%{targets} (run tar -xOf %%{deps} %s.txt))))
(rule (targets %s.res) (deps %s.tar) (action (with-stdout-to %%{targets} (run tar -xOf %%{deps} %s.res))))

(rule (targets %s.tar) (deps %s.log)
 (action (with-stdout-to %%{targets}
    (run docker run --privileged --rm -v opam2-archive:/home/opam/.opam/download-cache %s obi-ci-install %s)))) |}
      f f package f f package f f package f base base package

  let collect ~ov ~distro ~arch ~rev packages : string =
    let f =
      Fmt.strf "linux-%s-%s-%s" (D.tag_of_distro distro)
        (OV.string_of_arch arch) rev
    in
    let pkg ext p =
      Fmt.strf "linux-%s-%s-%s-%s.%s" (D.tag_of_distro distro)
        (OV.string_of_arch arch) p rev ext
    in
    let json_deps = String.concat " " (List.map (pkg "json") packages) in
    let txt_deps = String.concat " " (List.map (pkg "txt") packages) in
    let res_deps = String.concat " " (List.map (pkg "res") packages) in
    Fmt.strf
      {|
(rule (targets %s.tar.bz2) (deps %s %s %s) (action (run tar -jcf %%{targets} %%{deps})))
|}
      f json_deps txt_deps res_deps

  let gen ~ov ~distro ~arch ~rev packages =
    [ base_dockerfile ~ov ~distro ~arch ~rev
    ; base_build ~ov ~distro ~arch ~rev
    ; collect ~ov ~distro ~arch ~rev packages ]
    @ List.map (build_one ~ov ~distro ~arch ~rev) packages
    |> String.concat "\n"
end

let packages ~rev () =
  OS.Cmd.(
    run_out
      Cmd.(
        v "docker" % "run" % "--rm" % "ocaml/opam2:alpine" % "sh" % "-c"
        % Fmt.strf
            "git -C /home/opam/opam-repository pull -q origin master && git \
             checkout -q %s && opam update >/dev/null && opam list -s -a"
            rev)
    |> to_lines)

let gen_bulk_rules {ov; distro} () =
  let rev = "master" in
  let arch = `X86_64 in
  prerr_endline "Generating package list..." ;
  packages ~rev ()
  >>= fun packages ->
  prerr_endline (Fmt.strf "... %d." (List.length packages)) ;
  Rules.gen ~ov ~distro ~arch ~rev packages
  |> fun dune -> prerr_endline dune ; Ok ()

open Cmdliner

let setup_logs = C.setup_logs ()

let fpath = Arg.conv ~docv:"PATH" (Fpath.of_string, Fpath.pp)

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
    value & opt string "4.07.1"
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
  let doc = "generate bulk build dune rules for a particular opam repo rev" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P "TODO"] in
  ( Term.(term_result (const gen_bulk_rules $ build_t $ setup_logs))
  , Term.info "rules" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )

let default_cmd =
  let doc = "TODO" in
  let sdocs = Manpage.s_common_options in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
  , Term.info "obi-dune" ~version:"%%VERSION%%" ~doc ~sdocs )

let cmds = [opam_cmd]

let () = Term.(exit @@ eval_choice default_cmd cmds)
