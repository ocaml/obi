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

let home_dir () = OS.Env.(value "HOME" path ~absent:(Fpath.v "."))

let obi_dir () =
  let absent = Fpath.(home_dir () / ".obi") in
  OS.Env.(value ~log:Logs.Debug "OBI_HOME" path ~absent)

let remote_logs_repo () =
  let absent = "https://github.com/ocaml/obi-logs.git" in
  OS.Env.(value ~log:Logs.Debug "OBI_LOGS_REPO" string ~absent)

let logs_polling_interval = 3600.

let run_git_in_repo ~repo args =
  OS.Cmd.(run Cmd.(v "git" % "-C" % p repo %% of_list args))

let run_git args = OS.Cmd.(run Cmd.(v "git" %% of_list args))

let rec init ?(retry= false) ?(refresh= `Poll) () =
  let d = obi_dir () in
  Logs.info (fun l -> l "Initialising in %a" Fpath.pp d) ;
  OS.Dir.create ~path:true d
  >>= fun _ ->
  let local_logs_repo = Fpath.(d / "obi-logs") in
  let local_logs_mtime = Fpath.(d / "last-pulled") in
  OS.Dir.exists local_logs_repo
  >>= fun repo_exists ->
  ( if repo_exists then (
    let refresh =
      match (retry, refresh) with
      | _, `Local -> `Local
      | _, `Network -> `Network
      | true, `Poll -> `Network
      | false, `Poll ->
          let poll =
            OS.Path.stat local_logs_mtime
            >>= fun stats ->
            let curtime = Unix.gettimeofday () in
            let mtime = stats.Unix.st_mtime in
            if curtime -. mtime > logs_polling_interval then (
              Logs.debug (fun l ->
                  l "Obi logs repo polling time exceeded; pulling from network"
              ) ;
              Ok `Network )
            else (
              Logs.debug (fun l ->
                  l
                    "Obi logs repo has been pulled recently so not pulling \
                     from network" ) ;
              Ok `Local )
          in
          match poll with
          | Ok r -> r
          | Error (`Msg m) ->
              Logs.debug (fun l -> l "Forcing network poll due to: %s" m) ;
              `Network
    in
    match refresh with
    | `Network ->
        Logs.debug (fun l -> l "Fetching latest Obi logs") ;
        run_git_in_repo ~repo:local_logs_repo
          ["fetch"; "-q"; remote_logs_repo (); "index"]
        >>= fun () ->
        run_git_in_repo ~repo:local_logs_repo
          ["remote"; "set-url"; "origin"; remote_logs_repo ()]
        >>= fun () ->
        run_git_in_repo ~repo:local_logs_repo
          ["reset"; "-q"; "--hard"; "origin/index"]
        >>= fun () -> OS.File.write local_logs_mtime ""
    | `Local ->
        Logs.debug (fun l -> l "Using existing Obi logs") ;
        Ok () )
  else
    match refresh with
    | `Network | `Poll ->
        Logs.debug (fun l -> l "Cloning fresh Obi logs") ;
        run_git
          [ "clone"
          ; "-q"
          ; "--depth=1"
          ; "-b"
          ; "index"
          ; remote_logs_repo ()
          ; Cmd.p local_logs_repo ]
    | `Local ->
        Logs.err (fun l ->
            l
              "Must use --refresh=poll or --refresh=network to fetch initial \
               logs" ) ;
        Error
          (`Msg
            "Must use --refresh=poll or --refresh=network to fetch initial logs")
  )
  >>= fun () ->
  (* TODO store multiple versions based on date? *)
  let state = Fpath.(local_logs_repo / "index.sxp") in
  Logs.debug (fun l -> l "Parsing state file %a" Fpath.pp state) ;
  OS.File.read state
  >>= fun s ->
  try
    let t = Obi.Index.t_of_sexp (Sexplib.Sexp.of_string s) in
    let pkgs = t.Obi.Index.packages in
    Logs.debug (fun l -> l "Found metadata for %d packages" (List.length pkgs)) ;
    Ok pkgs
  with exn ->
    try
      Printexc.record_backtrace true ;
      (* Manually parse the sexpression to get the version string *)
      let version =
        let open Sexplib.Sexp in
        let s = of_string s in
        match s with
        | List (List [Atom "version"; Atom v] :: _) -> int_of_string v
        | _ -> raise (Failure "unable to find version string in index.sxp")
      in
      let err =
        Fmt.strf
          "Your opam-ci client is out of date.\n\
           It is using Obi metadata version %d but the upstream logs have \
           version %d (higher indicates a newer version).\n\
           Please run `opam update -u` to get the latest version of opam-ci \
           that is compatible with the latest log format.\n\
           If that does not work, you can try pinning to the development \
           version of opam-ci via `opam pin add opam-ci --dev`.\n\
           If that also does not help, then please report an issue at \
           https://github.com/ocaml/obi/issues"
          Obi.Index.current_version version
      in
      if retry then Error (`Msg err) else init ~retry:true ~refresh ()
    with exn ->
      let err =
        Fmt.strf
          "We have encountered a total failure to parse the upstream metadata.\n\
           Please run `opam update -u` to get the latest version of opam-ci \
           that is compatible with the latest log format.\n\
           If that does not work, try pinning to the development \
           version of opam-ci via `opam pin add opam-ci --dev`.\n\
           Please report this issue with at \
           https://github.com/ocaml/obi/issues with this backtrace:\n\
           %s\n\n         \
           %s\n"
          (Printexc.to_string exn)
          (Printexc.get_backtrace ())
      in
      if retry then Error (`Msg err) else init ~retry:true ~refresh ()
