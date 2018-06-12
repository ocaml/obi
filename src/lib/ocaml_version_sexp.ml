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

open Ocaml_version

let sexp_of_t t = Sexplib.Sexp.Atom (to_string t)

let t_of_sexp t =
  match t with
  | Sexplib.Sexp.Atom t -> of_string_exn t
  | _ -> failwith "invalid input for Ocaml_version.t_of_sexp"

let sexp_of_arch t = Sexplib.Sexp.Atom (string_of_arch t)

let arch_of_sexp t =
  match t with
  | Sexplib.Sexp.Atom t -> (
    match arch_of_string t with
    | Ok a -> a
    | Error (`Msg m) ->
        failwith ("invalid input for Ocaml_version.arch_of_sexp: " ^ m) )
  | _ -> failwith "invalid input for Ocaml_version.arch_of_sexp"
