open Sexplib.Conv

type node = {
  hostname: string;
  arch: string;
  ncpus: int32;
} [@@deriving sexp]

type t = {
  nodes: (int, node) Hashtbl.t;
  mutable last_id : int;
} [@@deriving sexp]

let v () =
  let nodes = Hashtbl.create 7 in
  let last_id = 1 in
  { nodes; last_id}

let register ~hostname ~arch ~ncpus t =
  let node = { hostname; arch; ncpus } in
  let id = t.last_id in
  t.last_id <- t.last_id + 1;
  Hashtbl.add t.nodes id node;
  id
