module Api = Worker_api.MakeRPC(Capnp_rpc_lwt)

open Lwt.Infix
open Capnp_rpc_lwt

module Client = struct
  module Register = Api.Client.Register

  let ping t msg =
    let open Register.Ping in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.msg_set params msg;
    Capability.call_for_value_exn t method_id request >|= Results.reply_get

  let heartbeat t msg callback =
    let open Register.Heartbeat in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.msg_set params msg;
    Params.callback_set params (Some callback);
    Capability.call_for_unit_exn t method_id request
end

module Callback = struct
  module Callback = Api.Client.Callback

  let log t msg =
    let open Callback.Log in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.msg_set params msg;
    Capability.call_for_unit t method_id request
end


let notify callback msg =
  let rec loop = function
    | 0 -> Capability.dec_ref callback; Lwt.return_unit
    | i ->
      Callback.log callback msg >>= function
      | Error ex ->
        Fmt.epr "Callback failed: %a@." Capnp_rpc.Error.pp ex;
        loop 0
      | Ok () ->
        Lwt_unix.sleep 1.0 >>= fun () ->
        loop (i - 1)
  in
  loop 3

let callback_service fn =
  let module Callback = Api.Service.Callback in
  Callback.local @@ object
    inherit Callback.service

    method log_impl params release_param_caps =
      let open Callback.Log in
      let msg = Params.msg_get params in
      release_param_caps ();
      fn msg;
      Service.return_empty ()
  end

let service =
  let module Register = Api.Service.Register in
  Register.local @@ object
    inherit Register.service

    method ping_impl params release_param_caps =
      let open Register.Ping in
      let msg = Params.msg_get params in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.reply_set results ("echo:" ^ msg);
      Service.return response

    method heartbeat_impl params release_params =
      let open Register.Heartbeat in
      let msg = Params.msg_get params in
      let callback = Params.callback_get params in
      release_params ();
      match callback with
      | None -> Service.fail "No callback parameter!"
      | Some callback ->
        Lwt.async (fun () -> notify callback msg);
        Service.return_empty ()

    method logger_impl _ release_params =
      let open Register.Logger in
      release_params ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.callback_set results (Some (callback_service prerr_endline));
      Service.return response

  end

let logger t =
  let open Client.Register.Logger in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_caps t method_id request Results.callback_get_pipelined