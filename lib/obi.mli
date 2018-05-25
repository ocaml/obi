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

module Index : sig
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
      build_result: [`Signaled of int | `Exited of int | `Uninstallable of string ];
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

module Builds : sig
  type build_result = {
    code: [`Signaled of int | `Exited of int | `Uninstallable of string ];
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

module VersionCompare : sig
  val compare : string -> string -> int
end
