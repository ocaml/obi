let installs j =
  let open Ezjsonm in
  try begin
    get_dict j |> List.assoc "results" |>
    get_list (fun v ->
    try
      let d = get_dict v in
      let action = List.assoc "action" d in
      let build = get_dict action |> List.assoc "build" |> get_dict in
      let name =  List.assoc "name" build |> get_string in
      let version = List.assoc "version" build |> get_string in
      let result = List.assoc "result" d in
      let is_error =
        try get_dict result |> List.mem_assoc "process-error"
        with _ -> false in
      let is_ok =
        try get_string result = "OK" with _ -> false in
      let is_skipped =
        try get_dict result |> List.mem_assoc "aborted"
        with _ -> false in
      let status =
        if is_error then `Fail else
          if is_ok then `Ok else
            if is_skipped then `Skipped else
              raise (Invalid_argument "Unable to parse JSON") in
      Some (name, version, status)
    with Not_found -> None
    ) |> List.fold_left (fun a -> function None -> a | Some x -> x::a) []
  end with
  | Not_found -> []
