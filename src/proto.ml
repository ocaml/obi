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
    let v impl =
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
          Memory_log.(send ~fd:Stdout ~id ~msg impl);
          Service.return_empty ()

        method close_impl params release_param_caps =
          let open Log.Close in
          let id = Params.id_get params in
          release_param_caps ();
          Memory_log.(close ~exit_code:1l ~id impl);
          Service.return_empty ()
      end
  end
end

module BuildLog = struct
  module Client = struct
    module BuildLog = Api.Client.BuildLog
    let stdout ~msg t =
      let open BuildLog.Stdout in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.msg_set params msg;
      Capability.call_for_unit_exn t method_id request

    let stderr ~msg t =
      let open BuildLog.Stderr in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.msg_set params msg;
      Capability.call_for_unit_exn t method_id request

    let close ~exit_code t =
      let open BuildLog.Close in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.exit_code_set params exit_code;
      Capability.call_for_unit_exn t method_id request
  end

  module Service = struct
    let v ~label t =
      let id = Memory_log.init ~label t in
      let module BuildLog = Api.Service.BuildLog in
      BuildLog.local @@ object
        inherit BuildLog.service

        method stdout_impl params release_param_caps =
          let open BuildLog.Stdout in
          let msg = Params.msg_get params in
          release_param_caps ();
          Memory_log.(send ~fd:Stdout ~id ~msg t);
          Service.return_empty ()

        method stderr_impl params release_param_caps =
          let open BuildLog.Stderr in
          let msg = Params.msg_get params in
          release_param_caps ();
          Memory_log.(send ~fd:Stderr ~id ~msg t);
          Service.return_empty ()

        method close_impl params release_param_caps =
          let open BuildLog.Close in
          let exit_code = Params.exit_code_get params in
          release_param_caps ();
          Memory_log.close ~exit_code ~id t;
          Service.return_empty ()
      end
  end
end

module Build = struct
  module Client = struct
    module Build = Api.Client.Build
    let shell ~cmd ~log t =
      let open Build.Shell in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.cmd_set params cmd;
      Params.log_set params (Some log);
      Capability.call_for_unit_exn t method_id request
  end
  module Service = struct
    let v exec_fn =
      let module Build = Api.Service.Build in
      Build.local @@ object
        inherit Build.service
        method shell_impl params release_param_caps =
          let open Build.Shell in
          let cmd = Params.cmd_get params in
          let log = Params.log_get params in
          release_param_caps ();
          match log with
          | None -> Service.fail "no logger specified"
          | Some log ->
            exec_fn ~log ~cmd;
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

    let worker ~hostname ~arch ~ncpus ~exec t =
      let open Register.Worker in
      let request, params = Capability.Request.create Params.init_pointer in
      Params.hostname_set params hostname;
      Params.arch_set params arch;
      Params.ncpus_set params ncpus;
      Params.exec_set params (Some exec);
      Capability.call_for_value_exn t method_id request >|= Results.logger_get
  end

  module Service = struct
    let v register_fn log_service logger_impl =
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
          let exec = Params.exec_get params in
          release_param_caps ();
          match exec with
          | None -> Service.fail "no exec callback found"
          | Some exec ->
            let _id = register_fn ~hostname ~arch ~ncpus ~exec in (* TODO id *)
            let response, results = Service.Response.create Results.init_pointer in
            Results.logger_set results (Some log_service); 
            Service.return response

        method list_logs_impl params release_param_caps =
          let open Register.ListLogs in
          release_param_caps ();
          let response, results = Service.Response.create Results.init_pointer in
          let logs = Array.of_list (Memory_log.list logger_impl) in
          let logs_p = Results.logs_init results (List.length logs) in
          Results.logs_set_list results [];
          Service.fail "todo"

      end
  end
end
