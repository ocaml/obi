open Lwt.Infix
open Capnp_rpc_lwt
module P = Proto
 
let serve addr : unit =
  let logger =
    let close id log = Fmt.pr "Closed %Lu:\n%s\n---\n" id (Sexplib.Sexp.to_string_hum (Memory_log.sexp_of_log log)) in
    let append id fd buf msg = Fmt.pr "Event %Lu: %s\n" id msg in
    Memory_log.v ~close ~append () in
  let logger_service = Proto.Log.Service.v logger in
  let nodes = Worker.v () in
  let register_fn ~hostname ~arch ~ncpus ~exec =
    let id, node = Worker.register ~hostname ~arch ~ncpus ~exec nodes in
    let job () = Lwt.async (fun () ->
      Lwt_unix.sleep 1.0 >>= fun () ->
      let label = Fmt.strf "%s/%s" hostname arch in
      let log = Proto.BuildLog.Service.v ~label logger in
      P.Build.Client.shell ~cmd:"sup" ~log node.Worker.exec >>= fun _ ->
      Lwt.return_unit)
    in
    let _ = job () in
    let _ = Lwt_unix.sleep 1.0 >>= fun () -> job (); Lwt.return_unit in
    ()
  in
  
  let offer = Proto.Register.Service.v register_fn logger_service logger in
  Lwt_main.run @@ Capnp_rpc_unix.serve ~offer addr

let worker_builder ~label =
  let exec_fn ~log ~cmd =
    Fmt.pr "exec_fn: %s -> %s\n" label cmd;
    P.BuildLog.Client.stdout ~msg:"wassup" log >>= fun () ->
    Lwt_unix.sleep 0.5 >>= fun () ->
    P.BuildLog.Client.stderr ~msg:"stderr stuff" log >>= fun () ->
    Lwt_unix.sleep 0.5 >>= fun () ->
    P.BuildLog.Client.close ~exit_code:0l log
  in
  P.Build.Service.v exec_fn

let connect addr label =
  let exec = worker_builder ~label in 
  let module P = Proto.BuildLog.Client in
  Lwt_main.run begin
    Lwt_switch.with_switch @@ fun switch ->
    let l = Capnp_rpc_unix.connect ~switch addr in
    Logs.info (fun f -> f "Connecting to service");
    let reg () =
      let hostname = label in
      let arch = "x86_64" in
      let ncpus = Uint32.of_int 2 in
      Proto.Register.Client.worker ~hostname ~arch ~ncpus ~exec l >>= function
      | None -> Lwt_unix.sleep 100.0
      | Some c -> Lwt_unix.sleep 100.0
    in
    reg ()
  end

let list_logs addr =
  Lwt_main.run begin
    Lwt_switch.with_switch @@ fun switch ->
    let l = Capnp_rpc_unix.connect ~switch addr in
    P.Register.Client.list_logs l >>= fun ents ->
    List.iter (fun (id,label) -> Fmt.pr "%Lu %s\n" id label) ents;
    Lwt.return_unit
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

let list_cmd =
  Term.(const list_logs $ connect_addr),
  let doc = "list all the logs available" in
  Term.info "list-logs" ~doc

let default_cmd =
  let doc = "a logger service example" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "logger" ~version:"v0.1" ~doc

let cmds = [serve_cmd; connect_cmd; list_cmd]

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
