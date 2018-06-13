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

open User
open Cmdliner

let setup_logs =
  let setup_log level =
    Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty () ;
    Logs.set_level level ;
    Logs.set_reporter (Logs_fmt.reporter ())
  in
  let global_option_section = "COMMON OPTIONS" in
  Term.(const setup_log $ Logs_cli.level ~docs:global_option_section ())

let copts_t =
  let docs = Manpage.s_common_options in
  let refresh =
    let doc =
      "How to refresh status logs. $(docv) defaults to $(i,poll) which pulls \
       from GitHub every hour. $(i,network) will force the metadata to be \
       pulled, and $(i,local) will only use the cached version locally."
    in
    let open Arg in
    let term =
      Arg.enum [("local", `Local); ("poll", `Poll); ("network", `Network)]
    in
    value & opt term `Poll & info ["refresh"] ~docv:"REFRESH_LOGS" ~doc ~docs
  in
  Term.(const copts $ refresh)

let filter_t =
  let maintainer =
    let doc =
      "Match packages via a case-insensitive substring check on the \
       $(b,maintainer) and $(b,tags) fields in the opam metadata. $(docv) can \
       be repeated multiple times to include more filters."
    in
    let open Arg in
    value & opt_all string []
    & info ["maintainer"; "m"] ~docv:"MAINTAINER" ~doc
  in
  let filters_t =
    let doc =
      "Filter the list of packages displayed. $(docv) defaults to \
       $(i,failures) to show all packages with errors. $(i,recent) will show \
       results for the latest version of each package. $(i,variants) will \
       list packages that regress with a compiler variant such as safe-string \
       or flambda. $(i,variants:fl), $(i,variants:rc) and $(i,variants:ss) \
       will show failures for just the flambda, release-candidate or \
       safe-string tests respectively. $(i,lagging) will show the packages \
       whose latest release is uninstallable against the latest version of \
       the compiler. $(i,orphaned) will list all the packages that do not \
       have an active maintainer, and so are looking for community help to \
       take care of them. $(i,all) will show all known results for all \
       versions including successes."
    in
    let open Arg in
    let term =
      Arg.enum
        [ ("all", `All)
        ; ("failures", `Failures)
        ; ("recent", `Recent)
        ; ("lagging", `Lagging)
        ; ("orphaned", `Orphaned)
        ; ("variants:ss", `Variants `SS)
        ; ("variants:fl", `Variants `Flambda)
        ; ("variants:rc", `Variants `RC) ]
    in
    value & opt term `Failures & info ["filter"; "f"] ~docv:"FILTERS" ~doc
  in
  Term.(const filters $ maintainer $ filters_t)

let param_t =
  let distro =
    let doc = "List only logs relating to this distribution" in
    let open Arg in
    let term =
      conv ~docv:"DISTRO"
        ( (fun d ->
            match D.distro_of_tag d with
            | Some r -> Ok r
            | None -> Error (`Msg ("unknown distribution " ^ d)) )
        , fun ppf d -> Fmt.pf ppf "%s" (D.tag_of_distro d) )
    in
    value & opt (some term) None & info ["distro"] ~docv:"DISTRO" ~doc
  in
  let ov =
    let doc = "List only logs relating to this OCaml compiler version" in
    let open Arg in
    let term = conv ~docv:"COMPILER" (OV.of_string, OV.pp) in
    value & opt (some term) None & info ["compiler"] ~doc
  in
  let arch =
    let doc = "List only logs relating to this CPU architecture" in
    let open Arg in
    let term =
      enum [("amd64", `X86_64); ("arm64", `Aarch64); ("ppc64le", `Ppc64le)]
    in
    value & opt (some term) None & info ["arch"] ~doc
  in
  Term.(const params $ distro $ ov $ arch)

let status_cmd =
  let doc = "summary of builds across compilers, OS and CPUs" in
  let exits = Term.default_exits in
  let packages_t =
    let doc =
      "optional list of package names (without versions) to filter the status \
       list by. If omitted, all packages matching the criteria are shown. You \
       can also use $(i,-m) to filter by maintainer or tag or $(i,-f) by \
       other filter criteria (see below)."
    in
    Arg.(value & pos_all string [] & info [] ~docv:"PACKAGES" ~doc)
  in
  let man =
    [ `S Manpage.s_description
    ; `P
        "The status view shows a panel of icons that represent different \
         combinations of ways to build opam packages. From left to right, \
         these are:"
    ; `I
        ( "$(b,Compiler)"
        , "The circled numbers represent OCaml compiler versions (a circled 6 \
           is OCaml 4.06, a circled 7 is 4.07, and so on). " )
    ; `I
        ( "$(b,Distro)"
        , "The square letters indicate different OS distributions. $(i,D) is \
           Debian, $(i,F) is Fedora, $(i,A) is Alpine, $(i,U) is Ubuntu and \
           $(i,S) is OpenSUSE." )
    ; `I
        ( "$(b,CPU Architecture)"
        , "The small circled letters represent different CPU architectures. \
           $(i,x) represents x86_64, $(i,a) is arm64 and $(i,p) is \
           PowerPC64LE." )
    ; `P
        "Some compiler variants are also tested to track down specific \
         problems, shown by the icons to the far right of the display."
    ; `I
        ( "$(b,safe-string)"
        , "The $(i,ss) icon is for 'safe-string' failures, which would happen \
           in OCaml 4.06 due to the switch to immutable strings." )
    ; `I
        ( "$(b,flambda)"
        , "The $(i,fl) icon is for packages that fail to compile with the \
           flambda variant of the compiler." )
    ; `I
        ( "$(b,release-candidate)"
        , "The $(i,flag) icon is for packages that fail to compile with the \
           latest release candidate of OCaml; this is useful to figure out \
           how much of the ecosystem works with a soon-to-be-released \
           compiler." )
    ; `P
        "The colours indicate the result of the build: $(i,white) indicates \
         the package was not built due to constraints, $(i,green) is a \
         successful build, $(i, yellow) indicates the build was skipped due \
         to a dependency failure, $(i, red) is a direct build failure of that \
         package, and $(i,magenta) and $(i,blue) indicate package metadata \
         errors such as a failure of the solver to find a solution or the \
         package sources being unavailable." ]
  in
  ( (let open Term in
    term_result
      (const show_status $ copts_t $ filter_t $ packages_t $ setup_logs))
  , Term.info "status" ~doc ~exits ~man )

let logs_cmd =
  let doc = "display detailed failure logs for opam packages" in
  let exits = Term.default_exits in
  let pkg_t = Arg.(required & pos 0 (some string) None & info [] ~doc) in
  let man =
    [ `S Manpage.s_description
    ; `P
        "The logs view will show the build details available for the various \
         configuration combinations.  To start, you can try to show the \
         results for a single package:"
    ; `P "opam-ci logs cdrom"
    ; `P
        "If just one failure is found, then the build logs are shown for that \
         failure. If there is more than one failure, the output will give you \
         a more precise command line to enter to select just one of the \
         failures.  For example:"
    ; `P
        "opam-ci logs cdrom.0.9.1 --compiler=4.06 --arch=amd64 \
         --distro=alpine-3.7"
    ; `P
        "There will often be repeat failures, so just pick one of them and \
         hopefully many of the other errors will be related." ]
  in
  ( (let open Term in
    term_result (const show_logs $ pkg_t $ copts_t $ param_t $ setup_logs))
  , Term.info "logs" ~doc ~exits ~man )

let default_cmd =
  let doc = "opam 2.0 package manager continuous integration interface" in
  let sdocs = Manpage.s_common_options in
  let man =
    [ `S Manpage.s_description
    ; `P
        "The $(tname) tool provides an interface to the opam 2.0 continuous\n  \
         integration cluster, which regularly rebuilds the package repository \
         across\n  \
         a variety of OCaml compiler versions, operating system distributions \
         and \n  \
         CPU architectures.  These builds are done regularly in remote \
         infrastructure and\n  \
         the results are pushed to a metadata repository where they are \
         fetched by this\n  \
         command to give you a summary of the status of your own packages."
    ; `P
        "The $(i,opam-ci status) command shows a dashboard of the build results\n   \
         across this matrix. Packages can be filtered by maintainer \
         substrings or tag names in the\n    \
         opam package description, so you see only those relevant to you."
    ; `P
        "The $(i,opam-ci logs) command will show you the build errors\n    \
         so you can fix them.  It also generates a Dockerfile of the precise \
         build to reproduce the environment locally for you."
    ; `P
        "To get started, try these commands with the maintainer argument \
         replaced with your own information or tags:"
    ; `P "# show all the failing MirageOS packages"
    ; `Noblank
    ; `P "opam-ci status -m org:mirage | less -R"
    ; `P "# show all the packages maintained by anil@recoil.org"
    ; `Noblank
    ; `P "opam-ci status -m anil@recoil.org --filter=all | less -R"
    ; `P
        "# show all the packages failing on the latest RC of the OCaml compiler"
    ; `Noblank
    ; `P "opam-ci status --filter=variants:rc | less -R"
    ; `P "# display all failure logs for the mirage-xen package"
    ; `Noblank
    ; `P "opam-ci logs mirage-xen"
    ; `P
        "See the $(b,--help) pages for the subcommands below for more \
         detailed information." ]
  in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
  , Term.info "opam-ci" ~version:"%%VERSION%%" ~doc ~sdocs ~man )

let cmds = [status_cmd; logs_cmd]

let () = Term.(exit @@ eval_choice default_cmd cmds)
