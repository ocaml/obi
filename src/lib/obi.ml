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

open Sexplib.Conv

module Ocaml_version = struct
  include Ocaml_version
  include Ocaml_version_sexp
end

module Builds = struct
  type build_result =
    { code: [`Signaled of int | `Exited of int]
    ; actions: string
    ; start_time: float
    ; end_time: float }
  [@@deriving sexp]

  type pkg = {name: string; versions: (string * build_result) list}
  [@@deriving sexp]

  type params =
    { arch: Dockerfile_distro.arch
    ; distro: Dockerfile_distro.t
    ; ov: Ocaml_version.t }
  [@@deriving sexp]

  type batch = {rev: string; params: params; pkgs: pkg list} [@@deriving sexp]
end

module Index = struct
  type maintainers = (string * string list) list [@@deriving sexp]

  type tags = (string * string list) list [@@deriving sexp]

  type deps = (string * string * [`Fail | `Ok | `Skipped]) list
  [@@deriving sexp]

  type result =
    [ `Ok
    | `Fail of int * deps
    | `Depfail of deps
    | `Uninstallable of string list
    | `No_sources of string list
    | `Solver_failure ]
  [@@deriving sexp]

  let pp_result ppf result =
    let open Format in
    match result with
    | `Ok -> pp_print_string ppf "ok"
    | `Fail (exit, deps) ->
        pp_print_string ppf "exit code " ;
        pp_print_int ppf exit
    | `Depfail deps -> pp_print_string ppf "dependency failed"
    | `Uninstallable sl ->
        pp_print_string ppf "uninstallable due to constraints"
    | `No_sources sl -> pp_print_string ppf "unable to fetch package sources"
    | `Solver_failure -> pp_print_string ppf "internal opam solver failure"

  type params =
    { arch: Dockerfile_distro.arch
    ; distro: Dockerfile_distro.t
    ; ov: Ocaml_version.t }
  [@@deriving sexp]

  type metadata =
    { rev: string
    ; params: params
    ; build_result: result
    ; start_time: float
    ; end_time: float
    ; log: string list }
  [@@deriving sexp]

  type pkg =
    { name: string
    ; mutable maintainers: string list
    ; mutable tags: string list
    ; mutable versions: (string * metadata list) list }
  [@@deriving sexp]

  type t = {version: int [@default 0]; packages: pkg list} [@@deriving sexp]

  let current_version = 1
end

module VersionCompare = VersionCompare
module OpamJsonActions = OpamJsonActions
