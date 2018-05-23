open Sexplib.Conv

module Ocaml_version = struct
  include Ocaml_version
  include Ocaml_version_sexp
end

module Builds = struct
  type build_result = {
    code: [`Signaled of int | `Exited of int];
    start_time: float;
    end_time: float;
  } [@@deriving sexp]

  type pkg =
    { name: string
    ; versions: (string * build_result) list }
    [@@deriving sexp]

  type params = {
    arch: Dockerfile_distro.arch;
    distro : Dockerfile_distro.t;
    ov: Ocaml_version.t;
  } [@@deriving sexp]

type batch =
  {rev: string; params: params; pkgs: pkg list}
  [@@deriving sexp]
end

module Index = struct
  type maintainers = (string * string) list [@@deriving sexp]
  type tags = (string * string list) list [@@deriving sexp]
  type params = {
    arch: Dockerfile_distro.arch;
    distro : Dockerfile_distro.t;
    ov: Ocaml_version.t;
  } [@@deriving sexp]
  type metadata =
    { rev: string;
      params: params;
      build_result: [`Signaled of int | `Exited of int ];
      start_time: float;
      end_time: float;
      log: string list;
  } [@@deriving sexp]
  type pkg =
   { name: string;
     mutable maintainers: string list;
     mutable tags: string list;
     mutable versions: (string * metadata list) list 
   } [@@deriving sexp]
  type pkgs = pkg list [@@deriving sexp]
end

module VersionCompare = VersionCompare
