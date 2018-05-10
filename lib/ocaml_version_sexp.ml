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
      | Error `Msg m ->
          failwith ("invalid input for Ocaml_version.arch_of_sexp: " ^ m) )
  | _ -> failwith "invalid input for Ocaml_version.arch_of_sexp"
