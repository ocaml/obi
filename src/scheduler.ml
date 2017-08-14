open Lwt.Infix
open Capnp_rpc_lwt
module P = Proto

module Node = struct

  type t = {
    hostname: string;
  } [@@deriving sexp]

end

let callback_fn msg =
  Fmt.pr "Callback got %S@." msg


let run_client service =
  let logger = P.logger service in
  P.Callback.log logger "Message from client" >|= function
  | Ok () -> ()
  | Error err -> Fmt.epr "Server's logger failed: %a" Capnp_rpc.Error.pp err

let socket_path = `Unix "/tmp/demo.socket"

let () =
  let server_thread = Capnp_rpc_unix.serve ~offer:P.service socket_path in
  let service = Capnp_rpc_unix.connect socket_path in
  Lwt_main.run @@ Lwt.pick [
    server_thread;
    run_client service;
  ]

