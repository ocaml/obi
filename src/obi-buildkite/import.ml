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
let tags = Hashtbl.create 100

let load_maintainers dir =
  let m = try
     Sexplib.Sexp.load_sexp_conv_exn Fpath.(dir / "maintainers.sxp" |> to_string) Obi.Index.maintainers_of_sexp
    with _ -> [] in
  List.iter (fun (k,v) -> Hashtbl.add maintainers k v) m

let save_maintainers dir =
  let f = Fpath.(dir / "maintainers.sxp" |> to_string) in
  let m = Hashtbl.fold (fun k v a -> (k,v)::a) maintainers [] in
  let m = List.sort (fun (a,_) (b,_) -> String.compare a b) m in
  Sexplib.Sexp.save_hum f (Obi.Index.sexp_of_maintainers m)

let load_tags dir =
  let m = try
     Sexplib.Sexp.load_sexp_conv_exn Fpath.(dir / "tags.sxp" |> to_string) Obi.Index.tags_of_sexp
    with _ -> Logs.info (fun l -> l "Unable to load tags cache"); [] in
  List.iter (fun (k,v) -> Hashtbl.add tags k v) m

let save_tags dir =
  let f = Fpath.(dir / "tags.sxp" |> to_string) in
  let m = Hashtbl.fold (fun k v a -> (k,v)::a) tags [] in
  let m = List.sort (fun (a,_) (b,_) -> String.compare a b) m in
  Sexplib.Sexp.save_hum f (Obi.Index.sexp_of_tags m)

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

let parse_string_list buf =
  let open Scanf in
  (* TODO Heuristic until https://github.com/ocaml/opam/issues/3365 is resolved *)
  match String.find (function ' ' | '"' -> true | _ -> false) buf with
  | Some _ -> 
     let rec fn ic = kscanf ic (fun _ _ -> []) "%S %r" fn (fun a b -> a::b) in
     fn (Scanning.from_string buf)
  | None -> [buf]

let find_tags pkg =
  match Hashtbl.find_opt tags pkg with
  | Some m -> Ok m
  | None ->
      OS.Cmd.(run_out (Cmd.(v "opam" % "info" % "-f" % "tags:" % pkg)) |> to_string) >>= fun t ->
      let t' = String.trim t |> parse_string_list in
      Hashtbl.add tags pkg t';
      Ok t'

let find_log logs_dir params rev pkg version =
  let open Obi.Builds in
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
  let open Obi.Builds in
  let rev = b.rev in
  let params = b.params in
  C.map (fun pkg ->
    let name = pkg.name in
    let ms = ref [] in
    let tags = ref [] in 
    C.map (fun (version, res) ->
      find_maintainer name >>= fun maintainer ->
      find_tags name >>= fun ts ->
      ms := maintainer :: !ms;
      List.iter (fun t -> tags := t :: !tags) ts;
      (match res.code with
       |`Exited 0 -> Ok []
       |_ -> begin
          match find_log logs_dir params rev name version with
          | Ok log -> Ok log
          | Error (`Msg e) ->
              Logs.warn (fun l -> l "Error log not found for %s.%s (%a): %s" name version Bos.OS.Cmd.pp_status res.code e);
              Ok [] end
      ) >>= fun log ->
           let build_result =
             match res.code with
             | `Exited 20 -> `Uninstallable log
             | n -> (n :> Obi.Index.result) in
           let params = { Obi.Index.arch=params.arch; ov=params.ov; distro=params.distro } in
           let metadata = [ { Obi.Index.params; rev; build_result; start_time=res.start_time; end_time=res.end_time; log } ] in
      Ok (version, metadata)
    ) pkg.versions >>= fun versions ->
    let maintainers = List.sort_uniq String.compare !ms in
    let tags = List.sort_uniq String.compare !tags in
    Ok { Obi.Index.name; versions; maintainers; tags}
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
            let p = { name; versions=[]; maintainers=[]; tags=[] } in
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
      p.tags <- List.sort_uniq String.compare (pkg.tags @ p.tags);
      let maintainers =
        List.fold_left
          (fun a m -> if List.mem m a then a else m::a) p.maintainers pkg.maintainers in
      p.maintainers <- maintainers;
    ) pkgs
  ) l;
  !r

let summarise input_dir (opam_dir:Fpath.t) =
  let meta_dir = Fpath.(input_dir / "metadata") in
  let logs_dir = Fpath.(input_dir / "logs") in
  Logs.info (fun l -> l "Loading maintainer cache");
  load_maintainers input_dir;
  Logs.info (fun l -> l "Loading tags cache");
  load_tags input_dir;
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
            let params = { Obi.Builds.arch; distro; ov } in
            let batch = Sexplib.Sexp.load_sexp_conv_exn file Obi.Builds.batch_of_sexp in
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
    let open Obi.Builds in
    let all_params = Hashtbl.fold (fun k (_,v) a -> if List.mem v.params a then a else v.params :: a) h [] in
    List.iter (fun p -> prerr_endline (ps sexp_of_params p)) all_params;
    (* find the batches with the latest opam revision for that param *)
    let latest = List.map (fun params ->
      Hashtbl.find_all by_param params |>
      List.sort (fun a b -> Rev.compare (Hashtbl.find revs a.rev) (Hashtbl.find revs b.rev)) |>
      List.find (fun b -> b.params = params)
    ) all_params in
    C.map (pkg_metadata_of_batch logs_dir) latest >>= fun latest ->
    let pkgs = merge_pkgs latest in
    save_maintainers input_dir;
    save_tags input_dir;
    print_endline (ps Obi.Index.sexp_of_pkgs pkgs);

    Ok ()
