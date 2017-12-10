open Rresult
open R.Infix
open Astring
open Bos
module C = Dockerfile_cmd
module OV = Ocaml_version

let store_build_log logs_dir log =
  let hash = Digest.(string log |> to_hex) in
  let f = Fpath.(logs_dir / (hash ^ ".txt")) in
  OS.File.exists f >>= function
  | true -> Ok hash
  | false ->
    OS.File.write f log >>= fun () ->
    Logs.debug (fun l -> l "Stored build log: %a" Fpath.pp f);
    Ok hash

let gather_logs force meta_dir logs_dir input_dir () = 
  if force then failwith "--force not implemented yet"; (* TODO *)
  OS.Dir.create logs_dir >>= fun _ ->
  OS.Dir.contents ~rel:true input_dir >>= fun revs ->
  C.iter (fun rev ->
    let p = Fpath.(input_dir // rev) in
    let rev = Fpath.to_string rev in
    Logs.info (fun l -> l "Processing git revision: %s" rev);
    OS.Dir.contents ~rel:true p >>= fun arches ->
    C.map (fun arch ->
      let p = Fpath.(p // arch) in
      Fpath.to_string arch |> OV.arch_of_string >>= fun arch ->
      OS.Dir.contents ~rel:true p >>= fun distros ->
      C.map (fun distro ->
        let p = Fpath.(p // distro) in
        Fpath.to_string distro |> Dockerfile_distro.distro_of_tag |>
        R.of_option ~none:(fun () ->
          R.error_msg (Fmt.strf "invalid distro %a" Fpath.pp distro)) >>= fun distro ->
        let h = Hashtbl.create 1000 in
        OS.Dir.contents ~rel:true p >>= fun ovs ->
        C.iter (fun ov ->
          let p = Fpath.(p // ov) in
          Fpath.to_string ov |> OV.of_string >>= fun ov ->
          OS.File.read_lines Fpath.(p / "pkgs.txt") >>= fun pkgs ->
          C.iter (fun pkg ->
            try begin
            Sexplib.Sexp.load_sexp Fpath.((p / (pkg ^ ".sxp")) |> to_string) |>
            C.cmd_log_of_sexp |> fun r ->
            match String.cut ~sep:"." pkg with
            | None -> R.error_msg (Fmt.strf "Package without full version found: %s" pkg)
            | Some (name, version) -> begin
               let status = r.C.status in
               store_build_log logs_dir r.C.stdout >>= fun log_hash ->
               let res = { Obi.status; log_hash } in
               let versions = if Hashtbl.mem h name then Hashtbl.find h name else [] in
               let results_for_ver = try List.assoc version versions with Not_found -> [] in
               let results_for_ver = (ov,res) :: results_for_ver in
               let versions = (version, results_for_ver) :: (List.remove_assoc version versions) in
               Hashtbl.replace h name versions;
               Ok ()
            end end with exn -> prerr_endline (Fmt.strf "warning: %s" (Printexc.to_string exn)); Ok ()
          ) pkgs
        ) ovs >>= fun () ->
        let versions = Hashtbl.fold (fun name versions acc ->
           let versions = List.sort (fun a b -> Obi.VersionCompare.compare (fst a) (fst b)) versions in
           {Obi.name;versions}::acc) h [] in
        Ok (arch,distro,versions)
      ) distros
    ) arches >>= fun res ->
    List.flatten res |> fun res ->
    let batch = { Obi.rev; res } in
    OS.Dir.create ~path:true Fpath.(meta_dir / "revs")  >>= fun _ ->
    OS.File.write Fpath.(meta_dir / "revs" / (rev ^ ".sxp")) (Sexplib.Sexp.to_string_hum (Obi.sexp_of_batch batch))
  ) revs

let generate_index meta_dir () =
   let opam_repo = "https://github.com/ocaml/opam-repository.git" in
   OS.Dir.contents Fpath.(meta_dir / "revs") >>= fun revs ->
   List.map (fun f -> Fpath.to_string f |> Sexplib.Sexp.load_sexp |> Obi.batch_of_sexp |> fun s -> s.Obi.rev) revs |> fun revs ->
   let tdir = Fpath.(v "_opam-repository") in
   let run_git args = OS.Cmd.(run_out (Cmd.(v "git" % "-C" % p tdir %% args)) |> to_string) in
   (OS.Dir.exists tdir >>= function
   | true ->
      Logs.info (fun l -> l "Refreshing opam repository");
      OS.Cmd.(run (Cmd.(v "git" % "-C" % p tdir % "pull")))
   | false ->
      Logs.info (fun l -> l "Cloning opam repository");
      OS.Cmd.(run (Cmd.(v "git" % "clone" % opam_repo % p tdir)))) >>= fun () ->
   C.map (fun rev ->
     run_git Cmd.(v "show" % "--pretty=format:%ct" % rev) >>= fun date ->
     String.trim date |> float_of_string |> fun date ->
     run_git Cmd.(v "show" % "--pretty=format:%s" % rev) >>= fun subj ->
     String.trim subj |> fun subj ->
     Ok (rev, date, subj)
   ) revs >>= fun revs ->
   List.sort (fun (_, a, _) (_, b, _) -> compare b a) revs |> fun revs ->
   let last_updated = Unix.gettimeofday () in
   let most_recent_rev,_,_ = List.hd revs in (* TODO check for empty rev list *)
   let index = {Obi.last_updated; most_recent_rev; revs} in
   OS.Dir.create meta_dir >>= fun _ ->
   OS.File.write Fpath.(meta_dir / "index.sxp") (Obi.sexp_of_index index |> Sexplib.Sexp.to_string_hum) 
