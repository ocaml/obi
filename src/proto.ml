module Api = Worker_api.MakeRPC(Capnp_rpc_lwt)

open Lwt.Infix
open Capnp_rpc_lwt

module Log = struct

  module Impl = struct

    type t = {
      logs : (int64, Buffer.t) Hashtbl.t;
      mutable last_id : int64;
      close : (int64 -> Buffer.t -> unit);
    }

    let v ~close () =
      let logs = Hashtbl.create 7 in
      let last_id = 1L in    
      { last_id; logs; close }

    let init ~label t =
      let id = t.last_id in
      t.last_id <- Int64.add t.last_id 1L;
      let buf = Buffer.create 1024 in
      Hashtbl.add t.logs id buf;
      id

    let send ~id ~msg t =
      match Hashtbl.find t.logs id with
      | buf -> Buffer.add_string buf msg
      | exception Not_found -> Fmt.pr "Not_found\n%!"; (* TODO log error *) ()

    let close ~id t =
      match Hashtbl.find t.logs id with
      | buf ->
          t.close id buf;
          Hashtbl.remove t.logs id
      | exception Not_found -> (* TODO log error *) ()
  end
  
  module Client = struct
    module Log = Api.Client.Log

    let init ~label t =
      let open Log.Init in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.label_set params label;
      Capability.call_for_value_exn t method_id request >|= Results.id_get

    let send ~id ~msg t =
      let open Log.Send in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.msg_set params msg;
      Params.id_set params id;
      Capability.call_for_unit_exn t method_id request

    let close ~id t =
      let open Log.Close in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.id_set params id;
      Capability.call_for_unit_exn t method_id request
  end
  
  module Service = struct

    let t impl =
      let module Log = Api.Service.Log in
      Log.local @@ object
        inherit Log.service

        method init_impl params release_param_caps =
          let open Log.Init in
          let label = Params.label_get params in
          release_param_caps ();
          let response, results = Service.Response.create Results.init_pointer in
          let id = Impl.init ~label impl in
          Results.id_set results id;
          Service.return response

        method send_impl params release_param_caps =
           let open Log.Send in
           let msg = Params.msg_get params in
           let id = Params.id_get params in
           release_param_caps ();
           Fmt.pr "send_impl: %Lu %s\n%!" id msg;
           Impl.send ~id ~msg impl;
           Service.return_empty ()
          
        method close_impl params release_param_caps =
          let open Log.Close in
          let id = Params.id_get params in
          release_param_caps ();
          Impl.close ~id impl;
          Service.return_empty ()
      end

  end
end

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