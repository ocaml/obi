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

let init () =
  let d = obi_dir () in
  Logs.info (fun l -> l "Initialising in %a" Fpath.pp d);
  OS.Dir.create ~path:true d >>= fun _ ->
  let local_logs_repo = Fpath.(d / "obi-logs") in
  OS.Dir.exists local_logs_repo >>= fun repo_exists ->
  (if repo_exists then begin
    Logs.debug (fun l -> l "Fetching latest Obi logs");
    run_git_in_repo ~repo:local_logs_repo ["fetch"; "origin"; "index"] >>= fun () ->
    run_git_in_repo ~repo:local_logs_repo ["reset"; "--hard"; "@{u}"]
  end else begin
    Logs.debug (fun l -> l "Cloning fresh Obi logs");
    run_git ["clone"; "--depth=1"; "-b"; "index"; remote_logs_repo (); Cmd.p local_logs_repo]
  end) >>= fun () ->
  (* TODO check version in obi-logs *)
  (* TODO store multiple versions based on date? *)
  let state = Fpath.(local_logs_repo / "index.sxp") in
  Logs.debug (fun l -> l "Parsing state file %a" Fpath.pp state);
  OS.File.read state >>= fun s ->
  let pkgs = Obi.Index.pkgs_of_sexp (Sexplib.Sexp.of_string s) in
  Logs.debug (fun l -> l "Found metadata for %d packages" (List.length pkgs));
  Ok pkgs