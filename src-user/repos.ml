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

let home_dir () =
  OS.Env.(value "HOME" path ~absent:(Fpath.v "."))

let obi_dir () =
  let absent = Fpath.(home_dir () / ".obi") in
  OS.Env.(value ~log:Logs.Debug "OBI_HOME" path ~absent)

let remote_logs_repo () =
  let absent = "https://github.com/avsm/obi-logs.git" in
  OS.Env.(value ~log:Logs.Debug "OBI_LOGS_REPO" string ~absent)

let run_git_in_repo ~repo args =
  OS.Cmd.(run (Cmd.(v "git" % "-C" % p repo %% of_list args)))

let run_git args =
  OS.Cmd.(run (Cmd.(v "git" %% of_list args)))

let init ?(network=true) () =
  let d = obi_dir () in
  Logs.info (fun l -> l "Initialising in %a" Fpath.pp d);
  OS.Dir.create ~path:true d >>= fun _ ->
  let local_logs_repo = Fpath.(d / "obi-logs") in
  OS.Dir.exists local_logs_repo >>= fun repo_exists ->
  (if repo_exists then begin
    if network then begin
      Logs.debug (fun l -> l "Fetching latest Obi logs");
      run_git_in_repo ~repo:local_logs_repo ["fetch"; "-q"; "origin"; "index"] >>= fun () ->
      run_git_in_repo ~repo:local_logs_repo ["reset"; "-q"; "--hard"; "@{u}"]
      end else begin
      Logs.debug (fun l -> l "Using existing Obi logs");
      Ok ()
    end;
  end else begin
    if network then begin
      Logs.debug (fun l -> l "Cloning fresh Obi logs");
      run_git ["clone"; "-q"; "--depth=1"; "-b"; "index"; remote_logs_repo (); Cmd.p local_logs_repo]
    end else begin
      Logs.err (fun l -> l "Must use --network=true to fetch initial logs");
      Error (`Msg "Must use --network=true to fetch initial logs")
    end
  end) >>= fun () ->
  (* TODO check version in obi-logs *)
  (* TODO store multiple versions based on date? *)
  let state = Fpath.(local_logs_repo / "index.sxp") in
  Logs.debug (fun l -> l "Parsing state file %a" Fpath.pp state);
  OS.File.read state >>= fun s ->
  let pkgs = Obi.Index.pkgs_of_sexp (Sexplib.Sexp.of_string s) in
  Logs.debug (fun l -> l "Found metadata for %d packages" (List.length pkgs));
  Ok pkgs
