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
    | Sexplib.Sexp.Atom t -> begin
       match arch_of_string t with
       | Ok a -> a
       | Error (`Msg m) -> failwith ("invalid input for Ocaml_version.arch_of_sexp: " ^ m)
    end
    | _ -> failwith "invalid input for Ocaml_version.arch_of_sexp"
end

type build_result = {
  status: [ `Signaled of int | `Exited of int ];
  log_hash: string;
} [@@deriving sexp]

type pkg = {
  name: string;
  versions : (string * (Ocaml_version.t * build_result) list) list
} [@@deriving sexp]

type batch = {
  rev: string;
  res: (Ocaml_version.arch * Dockerfile_distro.t * pkg list) list;
} [@@deriving sexp]

type index = {
  last_updated: float;
  revs: (string * float * string) list
} [@@deriving sexp]
