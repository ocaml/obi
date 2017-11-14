open Sexplib.Conv

module Ocaml_version = struct
  include Ocaml_version

  let sexp_of_t t = Sexplib.Sexp.Atom (to_string t)

  let t_of_sexp t =
    match t with
    | Sexplib.Sexp.Atom t -> of_string t
    | _ -> failwith "invalid input for Ocaml_version.t_of_sexp"


  let sexp_of_arch t = Sexplib.Sexp.Atom (string_of_arch t)

  let arch_of_sexp t =
    match t with
    | Sexplib.Sexp.Atom t -> (
      match arch_of_string t with
      | Ok a -> a
      | Error `Msg m ->
          failwith ("invalid input for Ocaml_version.arch_of_sexp: " ^ m) )
    | _ -> failwith "invalid input for Ocaml_version.arch_of_sexp"

end

type build_result =
  {status: [`Signaled of int | `Exited of int]; log_hash: string}
  [@@deriving sexp]

type pkg =
  { name: string
  ; versions: (string * (Ocaml_version.t * build_result) list) list }
  [@@deriving sexp]

type batch =
  {rev: string; res: (Ocaml_version.arch * Dockerfile_distro.t * pkg list) list}
  [@@deriving sexp]

type index =
  { last_updated: float
  ; most_recent_rev: string
  ; revs: (string * float * string) list }
  [@@deriving sexp]

module VersionCompare = VersionCompare

module Analysis = struct
  module OV = Ocaml_version

  let calculate_stats_for_pkgs pkgs =
    let h = Hashtbl.create 1000 in
    List.fold_left (fun acc pkg -> List.map snd pkg.versions @ acc) [] pkgs
    |> List.flatten
    |> List.iter (fun (ov, res) ->
           if not (Hashtbl.mem h ov) then Hashtbl.add h ov (ref 0, ref 0) ;
           let ok, fail = Hashtbl.find h ov in
           if res.status = `Exited 0 then incr ok else incr fail )
    |> fun () ->
    Hashtbl.fold (fun ov (ok, fail) acc -> (ov, (!ok, !fail)) :: acc) h []
    |> List.sort (fun (a, _) (b, _) -> OV.compare a b)


  let select_arch_distro ~arch ~distro batch =
    List.fold_left
      (fun acc (arch, distro, v) ->
        match (arch, distro) with
        | arch', distro' when arch' = arch && distro = distro' -> v :: acc
        | _ -> acc)
      [] batch.res
    |> function
      | [hd] -> hd
      | [] -> failwith "No results found"
      | _ -> failwith "Multiple results found"


  let find_rev_in_batch ~rev batches =
    List.find (fun batch -> batch.rev = rev) batches


  let find_pkgs_in_batch ~arch ~distro ~rev batches =
    find_rev_in_batch ~rev batches |> select_arch_distro ~arch ~distro


  let latest_version pkg =
    List.rev pkg.versions |> List.hd |> fst

  let partition_two_ocaml_versions pkgs ov1 ov2 =
    let classify_one ov res =
      match List.assoc ov res with
      | {status= `Exited 0; log_hash} -> `Ok
      | _ -> `Fail
      | exception Not_found -> `Missing
    in
    List.map
      (fun pkg ->
        let lv = latest_version pkg in
        List.map
          (fun (version, res) ->
            let cl1 = classify_one ov1 res in
            let cl2 = classify_one ov2 res in
            (version, (cl1, cl2)))
          pkg.versions
        |> fun r -> (pkg.name, lv, r))
      pkgs

  let safe_string_errors_406 pkgs =
    let ov1 = OV.of_string "4.06.0" in
    let ov2 = OV.of_string "4.06.0+default-unsafe-string" in
    partition_two_ocaml_versions pkgs ov1 ov2
    |> List.map (fun (name, lv, res) ->
           List.fold_left
             (fun acc (version, cl) ->
               match cl with `Fail, `Ok -> version :: acc | _ -> acc)
             [] res
           |> fun r -> (name, lv, r) )
    |> List.map (fun (name, lv, versions) ->
         let latest_broken = List.mem lv versions in
         (name, latest_broken, versions) )
    |> List.filter (fun (name, latest_broken, versions) -> versions <> [])
    |> List.sort (fun (a,_,_) (b,_,_) -> compare a b)

end

type analysis = {safe_string_errors: unit (* TODO *)} [@@deriving sexp]
