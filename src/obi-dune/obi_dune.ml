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

module OV = Ocaml_version
open Astring
open Bos
open Rresult
open R.Infix

module Rules = struct

  let base ~rev =
    Fmt.strf
      {|
(alias (name bulk) (deps results.tar.bz2))
(rule (targets custom) (mode fallback) (action (write-file custom "LABEL obi.no-custom=true\n")))
(rule (targets rev) (mode fallback) (action (write-file rev %S)))
(rule (targets Dockerfile) (action (with-stdout-to %%{targets} (
    progn
      (echo "FROM ocaml/opam2:%%{read-lines:ov}\n")
      (echo "WORKDIR /home/opam/opam-repository\n")
      (echo "RUN git pull origin master\n")
      (echo "RUN git checkout %%{read-lines:rev}\n")
      (echo "RUN opam update -uy\n")
      (echo "RUN opam switch %%{read-lines:ov}\n")
      (echo "%%{read:custom}")
      (echo "RUN echo '%s' | base64 -d > /tmp/obi-ci-install\n")
      (echo "RUN mv /tmp/obi-ci-install /usr/bin/obi-ci-install\n")
      (echo "RUN sudo chmod a+x /usr/bin/obi-ci-install\n")))))
(rule (targets arch) (action (with-stdout-to arch (run uname -m))))
(rule (targets image-name)
 (action (write-file image-name "obi-%%{read-lines:distro}_ocaml-%%{read-lines:ov}_%%{read-lines:arch}_%%{read-lines:rev}")))
(rule (targets Dockerfile.log) (deps Dockerfile)
  (action (with-outputs-to Dockerfile.log (run docker build --pull -t %%{read:image-name} --rm --force-rm .)))) |} rev Scripts.obi_ci_install

  let build_one p : string =
    Fmt.strf
      {|
(rule (targets %s.json) (deps %s.tar) (action (with-stdout-to %%{targets} (run tar -xOf %%{deps} %s.json))))
(rule (targets %s.txt) (deps %s.tar) (action (with-stdout-to %%{targets} (run tar -xOf %%{deps} %s.txt))))

(rule (targets %s.tar) (deps Dockerfile.log)
 (action (with-stdout-to %%{targets}
    (run docker run --privileged --rm -v opam2-archive:/home/opam/.opam/download-cache %%{read:image-name} obi-ci-install %s)))) |}
      p p p p p p p p

  let collect packages : string =
    let pkg ext x = Fmt.strf "%s.%s" x ext in
    let json_deps = String.concat ~sep:" " (List.map (pkg "json") packages) in
    let txt_deps = String.concat ~sep:" " (List.map (pkg "txt") packages) in
    Fmt.strf
      {|
(rule (targets results.tar.bz2) (deps %s %s arch ov rev distro) (action (run tar -jcf %%{targets} %%{deps})))
|} json_deps txt_deps

  let gen ~rev packages =
    [ base ~rev
    ; collect packages ]
    @ List.map build_one packages
    |> String.concat ~sep:"\n"
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

let opam_repo_rev rev =
  match rev with
  | Some rev -> Ok rev
  | None -> begin
      OS.Cmd.(run_out Cmd.(v "git" % "ls-remote" % "https://github.com/ocaml/opam-repository.git" % "refs/heads/master") |> to_string) >>= fun l ->
      match String.cuts ~sep:"\t" l with
      | hd::_ -> Logs.debug (fun l -> l "opam repo rev %s" hd); Ok hd
      | _ -> Error (`Msg "unable to get remote head of opam-repository")
    end

let gen_bulk_rules rev () =
  opam_repo_rev rev >>= fun rev ->
  Logs.app (fun l -> l "Using opam repo rev %s" rev);
  OS.File.write Fpath.(v "rev") rev >>= fun () ->
  Logs.app (fun l -> l "Generating package list...") ;
  packages ~rev ()
  >>= fun packages ->
  Logs.app (fun l -> l "... %d packages found." (List.length packages));
  Rules.gen ~rev packages
  |> fun dune ->
  OS.File.exists Fpath.(v "dune") >>= function
  | true -> R.error_msg "`dune` file exists, please delete it and rerun"
  | false -> OS.File.write Fpath.(v "dune") dune

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

let opam_repo_rev_t =
  let doc = "opam repo git rev" in
  let open Arg in
  value & opt (some string) None
  & info ["opam-repo-rev"] ~docv:"OPAM_REPO_REV" ~doc

let opam_cmd =
  let doc = "generate bulk build dune rules for a particular opam repo rev" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P "TODO"] in
  ( Term.(term_result (const gen_bulk_rules $ opam_repo_rev_t $ setup_logs))
  , Term.info "rules" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )

let default_cmd =
  let doc = "TODO" in
  let sdocs = Manpage.s_common_options in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
  , Term.info "obi-dune" ~version:"%%VERSION%%" ~doc ~sdocs )

let cmds = [opam_cmd]

let () = Term.(exit @@ eval_choice default_cmd cmds)
