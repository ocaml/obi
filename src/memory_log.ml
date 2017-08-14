type t = {
  logs : (int64, Buffer.t) Hashtbl.t;
  mutable last_id : int64;
  append: (int64 -> Buffer.t -> string -> unit);
  close : (int64 -> Buffer.t -> unit);
}

let v ~append ~close () =
  let logs = Hashtbl.create 7 in
  let last_id = 1L in    
  { last_id; logs; append; close }

let init ~label t =
  let id = t.last_id in
  t.last_id <- Int64.add t.last_id 1L;
  let buf = Buffer.create 1024 in
  Hashtbl.add t.logs id buf;
  id

let send ~id ~msg t =
  match Hashtbl.find t.logs id with
  | buf ->
      Buffer.add_string buf msg;
      t.append id buf msg
  | exception Not_found ->
      Logs.err (fun l -> l "Send: id %Lu not found" id)

let close ~id t =
  match Hashtbl.find t.logs id with
  | buf ->
    t.close id buf;
    Hashtbl.remove t.logs id
  | exception Not_found ->
      Logs.err (fun l -> l "Close: id %Lu not found" id)
