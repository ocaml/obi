open Rresult
open R.Infix
open Astring
open Bos
module C = Dockerfile_cmd
module D = Dockerfile_distro
module OV = Ocaml_version

module Rev = struct
  open Sexplib.Conv
  type t = {
    rev: string;
    date: float;
    subject: string;
  } [@@deriving sexp]

  let print_date t =
    Ptime.of_float_s t.date |> (function None -> failwith "bad time" | Some d -> d) |>
    Ptime.to_rfc3339

  let print_rev t =
    String.with_range ~len:8 t.rev

  let compare a b = compare b.date a.date
end


let ps fn v = Sexplib.Sexp.to_string_hum (fn v)

let h = Hashtbl.create 10
let by_param = Hashtbl.create 10
let revs = Hashtbl.create 10
let maintainers = Hashtbl.create 100

let load_maintainers dir =
  let m = try
     Sexplib.Sexp.load_sexp_conv_exn Fpath.(dir / "maintainers.sxp" |> to_string) Obi.Index.maintainers_of_sexp
    with _ -> [] in
  List.iter (fun (k,v) -> Hashtbl.add maintainers k v) m

let save_maintainers dir =
  let f = Fpath.(dir / "maintainers.sxp" |> to_string) in
  let m = Hashtbl.fold (fun k v a -> (k,v)::a) maintainers []  in
  Sexplib.Sexp.save_hum f (Obi.Index.sexp_of_maintainers m)

let info_of_rev tdir rev =
   let run_git args = OS.Cmd.(run_out (Cmd.(v "git" % "-C" % p tdir %% args)) |> to_string) in
   run_git Cmd.(v "show" % "-s" % "--pretty=format:%ct" % rev) >>= fun date ->
   String.trim date |> float_of_string |> fun date ->
   run_git Cmd.(v "show" % "-s" % "--pretty=format:%s" % rev) >>= fun subj ->
   String.trim subj |> fun subject ->
   Ok {Rev.rev; date; subject}

let find_maintainer pkg =
  match Hashtbl.find_opt maintainers pkg with
  | Some m -> Ok m
  | None ->
    OS.Cmd.(run_out (Cmd.(v "opam" % "info" % "-f" % "maintainer:" % pkg)) |> to_string) >>= fun maintainer ->
    let m = String.trim maintainer in
    Hashtbl.add maintainers pkg m;
    Ok m

let find_log logs_dir params rev pkg version =
  let open Obi in
  let log = Printf.sprintf "%s/linux/%s/%s/%s/%s/%s.%s.txt" (Fpath.to_string logs_dir) (OV.string_of_arch params.arch) (D.tag_of_distro params.distro) (OV.to_string params.ov) rev pkg version in
  Bos.OS.File.read_lines (Fpath.v log) >>= fun lines ->
  (* Grab last lines or error report *)
  let rec chop_error_report = function
    | [] -> []
    | hd::tl when String.is_prefix ~affix:"<><> Error report" hd -> tl
    | hd::tl when String.is_prefix ~affix:"-_-_ Error report" hd -> tl
    | hd::tl  -> chop_error_report tl in
  let rec tail n acc = function
    | [] -> acc
    | hd::tl when n = 0 -> hd::acc
    | hd::tl -> tail (n-1) (hd::acc) tl
  in
  let log = List.rev lines |> chop_error_report |> tail 30 [] in
  Ok log

let find_latest_results srevs params =
  let r = ref None in
  let rev = List.find
    (fun rev ->
       Hashtbl.find_all h rev.Rev.rev |>
       List.find_opt (fun (p,_) ->
    (*     Printf.eprintf "comparing %s -> %s\n%!" (ps Obi.sexp_of_params p) (ps Obi.sexp_of_params params); *)
          p = params) |>
       function None -> false | Some (_,b) -> r := Some b; true
    ) srevs in
  match !r with
  | None -> failwith "distro not found"
  | Some v -> rev, v

let pkg_metadata_of_batch logs_dir b =
  let open Obi in
  let rev = b.rev in
  let params = b.params in
  C.map (fun pkg ->
    let name = pkg.name in
    C.map (fun (version, res) ->
      find_maintainer name >>= fun maintainer ->
      (match res.code with
       |`Exited 0 -> Ok []
       |_ -> find_log logs_dir params rev name version) >>= fun log ->
           let metadata = [ { Index.maintainer; params; rev; build_result=res.code; start_time=res.start_time; end_time=res.end_time; log } ] in
      Ok (version, metadata)
    ) pkg.versions >>= fun versions ->
    Ok { Index.name; versions}
  ) b.pkgs

let merge_pkgs (l:Obi.Index.pkgs list) =
  let open Obi.Index in
  let r = ref [] in
  List.iter (fun pkgs ->
    List.iter (fun pkg ->
      (* Initialise new package if no prev versions exist *)
      let name = pkg.name in
      let p =
        match List.find_opt (fun p -> p.name = name) !r with
        | None ->
           let p = { name; versions=[] } in
           r := p :: !r;
           p
        | Some p -> p
      in
      let versions =
        List.fold_left (fun versions (version, res) ->
          match List.assoc_opt version versions with
          | None -> (version, res)::versions
          | Some r ->
              let x = List.remove_assoc version versions in
              (version, (res@r))::x
        ) p.versions pkg.versions
      in
      p.versions <- versions;
    ) pkgs
  ) l;
  !r

let summarise input_dir (opam_dir:Fpath.t) =
  let meta_dir = Fpath.(input_dir / "metadata") in
  let logs_dir = Fpath.(input_dir / "logs") in
  Logs.info (fun l -> l "Loading maintainer cache");
  load_maintainers input_dir;
  OS.Dir.contents ~rel:true meta_dir >>=
  C.iter (fun os ->
    let dir = Fpath.(meta_dir // os) in
    Logs.info (fun l -> l "Found OS %a" Fpath.pp os);
    OS.Dir.contents ~rel:true dir >>=
    C.iter (fun arch ->
      let dir = Fpath.(dir // arch) in
      Logs.info (fun l -> l "Found CPU architecture %a" Fpath.pp arch);
      let arch = OV.arch_of_string_exn (Fpath.to_string arch) in
      OS.Dir.contents ~rel:true dir >>=
      C.iter (fun distro ->
        Logs.info (fun l -> l "Found distribution %a" Fpath.pp distro);
        let dir = Fpath.(dir // distro) in
        let distro = D.distro_of_tag (Fpath.to_string distro) |> function None -> failwith "unknown distro" | Some d -> d in
        OS.Dir.contents ~rel:true dir >>=
        C.iter (fun ov ->
          Logs.info (fun l -> l "Found OCaml version %a" Fpath.pp ov);
          let dir = Fpath.(dir // ov) in
          OV.of_string (Fpath.to_string ov) >>= fun ov ->
          OS.Dir.contents ~rel:true dir >>=
          C.iter (fun rev ->
            Logs.info (fun l -> l "Found git rev %a" Fpath.pp rev);
            let file = Fpath.(dir // rev |> to_string) in
            let params = { Obi.arch; distro; ov } in
            let batch = Sexplib.Sexp.load_sexp_conv_exn file Obi.batch_of_sexp in
            (if Hashtbl.mem revs batch.rev then Ok () else begin
              info_of_rev opam_dir batch.rev >>= fun info ->
                Hashtbl.replace revs batch.rev info; Ok () end) >>= fun () ->
            Hashtbl.add h batch.rev (params, batch);
            Hashtbl.add by_param params batch;
            Ok ()
          ))))) >>= fun () ->
    let srevs =
      Hashtbl.fold (fun k v a -> v::a) revs [] |>
      List.sort (fun a b -> compare b.Rev.date a.Rev.date) in
    List.iter (fun r ->
      let open Rev in
      Printf.eprintf "%s %s %s\n%!" (String.with_range ~len:8 r.rev) (print_date r) r.subject;
    ) srevs;
    Printf.eprintf "\n%!";
    (* figure out all the params available in recent build revisions *)
    let all_params = Hashtbl.fold (fun k (_,v) a -> if List.mem v.Obi.params a then a else v.Obi.params :: a) h [] in
    List.iter (fun p -> prerr_endline (ps Obi.sexp_of_params p)) all_params;
    (* find the batches with the latest opam revision for that param *)
    let latest = List.map (fun params ->
      Hashtbl.find_all by_param params |>
      List.sort (fun a b -> Rev.compare (Hashtbl.find revs a.Obi.rev) (Hashtbl.find revs b.Obi.rev)) |>
      List.find (fun b -> b.Obi.params = params)
    ) all_params in
    C.map (pkg_metadata_of_batch logs_dir) latest >>= fun latest ->
    let pkgs = merge_pkgs latest in
    save_maintainers input_dir;
    print_endline (ps Obi.Index.sexp_of_pkgs pkgs);

    Ok ()
