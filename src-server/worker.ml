open Sexplib.Conv

type 'a node = {
  hostname: string;
  arch: string;
  ncpus: int32;
  exec: 'a;
} [@@deriving sexp]

type 'a t = {
  nodes: (int, 'a node) Hashtbl.t;
  mutable last_id : int;
}

let v () =
  let nodes = Hashtbl.create 7 in
  let last_id = 1 in
  { nodes; last_id}

let build ~cmd =
  Printf.eprintf "%s: %s\n%!" "node" cmd;
  let exit_code = 1l in
  let stdout = "stdout" in
  let stderr = "stderr" in
  exit_code, stdout, stderr

let register ~hostname ~arch ~ncpus ~exec t =
  Logs.debug (fun l -> l "registering worker: %s %s %lu" hostname arch ncpus);
  let node = { hostname; arch; ncpus; exec } in
  let id = t.last_id in
  t.last_id <- t.last_id + 1;
  Hashtbl.add t.nodes id node;
  id, node
