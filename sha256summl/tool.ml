open Buffer
open Sys
open Filename
open Unix_cstruct
open Nocrypto.Hash

let rec map_dir f path =
  (* this does a lot of string concatenation
     and dir accessing - not optimized *)
  let rec _map_dir f path =
    let success = ref true in
    let pathlen = String.length path -1 in
    let path = if path.[pathlen] = '/' then
        String.sub path 0 pathlen
        else path in
    let _ = try 
    let dirarr = readdir path in
      Array.fast_sort compare dirarr;
      for i = 0 to Array.length dirarr -1 do
        if !success then
        (* TODO only add / if needed*)
        let dirent = path^"/"^ dirarr.(i) in
        success := map_dir f dirent 
      done 
    with
     error -> success := false
    in !success
  in
  if file_exists path then begin
  (*should check its not a socket or symlink etc? *)
    match basename path with
     | "" (* ends in / *)
     | _ when is_directory path
       -> _map_dir f path
     | _ -> f path
  end else
    false (* map not succsssful*)

let sha256sum file =
  let mmap_file = 
    Unix_cstruct.of_fd
      Unix.(openfile file [O_RDONLY] 0) in
  let hexbuf = Buffer.create (32*3) in
  let () = 
    Cstruct.hexdump_to_buffer
    hexbuf
    (SHA256.digest mmap_file)
  in
    String.trim
    (Buffer.contents hexbuf)
(* in Core_string.filter (function '\n'|' '->false|true) -- im too stupid to manage installation of Core *)

let path_and_hash file =
  (file, sha256sum file)

let print_hash file=
  let () =
  let (_, hash) = path_and_hash file in
  let acc = String.concat " " [file;hash] in
    print_endline acc 
  in true

let () =
(* Sys.set_signal Sys.sigusr1 print_progress *)

  if map_dir print_hash argv.(1) then
    print_endline "success"
  else
    print_endline "failure"
    (* should save a descripyion of what went wrong - exceptions? *)
