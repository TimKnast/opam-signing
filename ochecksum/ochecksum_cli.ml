open Ochecksum_lib (* todo fix naming *)
open Cmdliner

let arg_path =
  let doc = "The path to generate checksums for recursively" in
   Arg.(value & pos 0 string "\x00" & info [] ~docv:"PATH" ~doc)

let info =
  let doc =
    "Generate or verify checksums of files in a directory tree, failing if the directory structure is different from the original"
  and man = [] in
    Term.info "ochecksum" ~version:"0.0" ~doc ~man

let wrap_ochecksum a =
  match ochecksum ~path:a with
  | Some e -> print_endline e ;
              exit 1
  | None -> print_endline "success."

let term_generate =
    Term.(pure wrap_ochecksum $ arg_path)

let () =
  let terms = (
    term_generate, info
  ) in match Term.eval terms with
  |  a -> exit 5

