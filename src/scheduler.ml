open Lwt.Infix
open Capnp_rpc_lwt
module P = Proto

module Node = struct

  type t = {
    hostname: string;
  } [@@deriving sexp]

end

let logger =
  let close id buf =
    Fmt.pr "Closed %Lu:\n%s\n---\n" id (Buffer.contents buf)
  in
  let append id buf msg =
    Fmt.pr "Event %Lu: %s\n" id msg
  in
  let impl = Memory_log.v ~close ~append () in
  Proto.Log.Service.t impl

let serve addr : unit =
  Lwt_main.run @@ Capnp_rpc_unix.serve ~offer:logger addr

let connect addr label =
  let module P = Proto.Log.Client in
  Lwt_main.run begin
    Lwt_switch.with_switch @@ fun switch ->
    let l = Capnp_rpc_unix.connect ~switch addr in
    Logs.info (fun f -> f "Connecting to log service");
    P.init ~label l >>= fun id ->
    Fmt.pr "Id: %Lu@." id;
    let rec send () =
      Lwt_io.(read stdin) >>= function
      | "" -> Lwt.return_unit
      | buf -> P.send ~id ~msg:buf l >>= send
    in
    send () >>= fun () ->
    P.close ~id l 
  end

open Cmdliner

let connect_addr =
  let i = Arg.info [] ~docv:"ADDR" ~doc:"Address of server, e.g. unix:/run/my.socket" in
  Arg.(required @@ pos 0 (some Capnp_rpc_unix.Connect_address.conv) None i)

let label_arg =
  Arg.(value & opt string "(default)" & info ["l";"label"] ~docv:"LABEL" ~doc:"Label to identify this log entry")

let listen_addr =
  let i = Arg.info [] ~docv:"ADDR" ~doc:"Address to listen on, e.g. unix:/run/my.socket" in
  Arg.(required @@ pos 0 (some Capnp_rpc_unix.Listen_address.conv) None i)

let serve_cmd =
  Term.(const serve $ listen_addr),
  let doc = "provide a Cap'n Proto logger service" in
  Term.info "serve" ~doc

let connect_cmd =
  Term.(const connect $ connect_addr $ label_arg),
  let doc = "connect to a Cap'n Proto logger service" in
  Term.info "connect" ~doc

let default_cmd =
  let doc = "a logger service example" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "logger" ~version:"v0.1" ~doc

let cmds = [serve_cmd; connect_cmd]

let pp_qid f = function
  | None -> ()
  | Some x ->
    let s = Uint32.to_string x in
    Fmt.(styled `Magenta (fun f x -> Fmt.pf f " (qid=%s)" x)) f s
  
let reporter =
  let report src level ~over k msgf =
    let src = Logs.Src.name src in
    msgf @@ fun ?header ?(tags=Logs.Tag.empty) fmt ->
    let qid = Logs.Tag.find Capnp_rpc.Debug.qid_tag tags in
    let print _ =
      Fmt.(pf stdout) "%a@." pp_qid qid;
      over ();
      k ()
    in
    Fmt.kpf print Fmt.stdout ("%a %a: @[" ^^ fmt ^^ "@]")
      Fmt.(styled `Magenta string) (Printf.sprintf "%11s" src)
      Logs_fmt.pp_header (level, header)
  in
  { Logs.report = report }

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter reporter;
  Logs.set_level ~all:true (Some Logs.Info);
  Term.eval_choice default_cmd cmds |> Term.exit