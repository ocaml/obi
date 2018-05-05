open Rresult
open R.Infix
open Astring
open Bos
module C = Dockerfile_cmd
module D = Dockerfile_distro
module OV = Ocaml_version

let gather_logs input_dir =
  let meta_dir = Fpath.(input_dir / "metadata") in
  let logs_dir = Fpath.(input_dir / "logs") in
  OS.Dir.contents ~rel:true meta_dir >>=
  C.iter (fun os ->
    let dir = Fpath.(meta_dir // os) in
    Logs.info (fun l -> l "Found OS %a" Fpath.pp os);
    OS.Dir.contents ~rel:true dir >>=
    C.iter (fun arch ->
      let dir = Fpath.(dir // arch) in
      Logs.info (fun l -> l "Found CPU architecture %a" Fpath.pp arch);
      let arch = OV.arch_of_string (Fpath.to_string arch) in
      OS.Dir.contents ~rel:true dir >>=
      C.iter (fun distro ->
        Logs.info (fun l -> l "Found distribution %a" Fpath.pp distro);
        let dir = Fpath.(dir // distro) in
        let distro = D.distro_of_tag (Fpath.to_string distro) in
        OS.Dir.contents ~rel:true dir >>=
        C.iter (fun ov ->
          Logs.info (fun l -> l "Found OCaml version %a" Fpath.pp ov);
          let dir = Fpath.(dir // ov) in
          OV.of_string (Fpath.to_string ov) >>= fun ov ->
          OS.Dir.contents ~rel:true dir >>=
          C.iter (fun rev ->
            Logs.info (fun l -> l "Found git rev %a" Fpath.pp rev);
            let file = Fpath.(dir // rev |> to_string) in
            let batch = Sexplib.Sexp.load_sexp_conv_exn file Obi.batch_of_sexp in
            Ok ()
          )
        )
      )
    )
  )
