
open Nocrypto.Hash

let sha256sum file =
  let mmap_file = 
    Unix_cstruct.of_fd
      Unix.(openfile file [O_RDONLY] 0)
  and hexbuf = Buffer.create (32*3) in
  let () = 
    Cstruct.hexdump_to_buffer
      hexbuf
      (SHA256.digest mmap_file)
  in
    strip_sha256_whitespace hexbuf
(* in Core_string.filter (function '\n'|' '->false|true) -- im too stupid to manage installation of Core *)

let () =
(* Sys.set_signal Sys.sigusr1 print_progress *)

(* map_dir should return an option of errors with Some having a description of what went wrong *)
  if map_dir print_hash argv.(1) then
    print_endline "success"
  else
    print_endline "failure"
    (* should save a descripyion of what went wrong - exceptions? *)
