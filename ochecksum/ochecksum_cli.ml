open Ochecksum_lib (* todo fix naming *)
open Cmdliner

let arg_path =
  let doc = "The path to generate checksums for recursively" in
   Arg.(value & pos 0 string "" & info [] ~docv:"PATH" ~doc)

let info =
  let doc =
    "Generate or verify checksums of files in a directory tree, failing if the directory structure is different from the original"
  and man = [] in
    Term.info "ochecksum" ~version:"0.0" ~doc ~man

let wrap_ochecksum a =
  let _ =
    ochecksum ~path:a
  in ()

let term_generate =
    Term.(pure wrap_ochecksum $ arg_path)

let () =
  let terms = (
    term_generate, info
  ) in match Term.eval terms with
  | _ -> exit 0

