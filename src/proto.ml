module Api = Worker_api.MakeRPC(Capnp_rpc_lwt)

open Lwt.Infix
open Capnp_rpc_lwt

module Log = struct
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
          let id = Memory_log.init ~label impl in
          Results.id_set results id;
          Service.return response

        method send_impl params release_param_caps =
          let open Log.Send in
          let msg = Params.msg_get params in
          let id = Params.id_get params in
          release_param_caps ();
          Fmt.pr "send_impl: %Lu %s\n%!" id msg;
          Memory_log.send ~id ~msg impl;
          Service.return_empty ()

        method close_impl params release_param_caps =
          let open Log.Close in
          let id = Params.id_get params in
          release_param_caps ();
          Memory_log.close ~id impl;
          Service.return_empty ()
      end
  end
end

module Register = struct
  module Client = struct
    module Register = Api.Client.Register
    let ping ~msg t =
      let open Register.Ping in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.msg_set params msg;
      Capability.call_for_value_exn t method_id request >|= Results.reply_get

    let worker ~hostname ~arch ~ncpus t =
      let open Register.Worker in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.hostname_set params hostname;
      Params.arch_set params arch;
      Params.ncpus_set params ncpus;
      Capability.call_for_value_exn t method_id request >|= Results.logger_get
  end

  module Service = struct
    let t nodes log_service =
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

        method worker_impl params release_param_caps =
          let open Register.Worker in
          let hostname = Params.hostname_get params in
          let arch = Params.arch_get params in
          let ncpus = Params.ncpus_get params |> Uint32.to_int32 in
          release_param_caps ();
          let id = Worker.register ~hostname ~arch ~ncpus nodes in
          let response, results = Service.Response.create Results.init_pointer in
          Results.logger_set results (Some log_service);
          Service.return response
      end
  end
end
