open Buffer
open Sys
open Filename
(* open Unix TODO for proper openat *)
open Unix_cstruct
open Nocrypto.Hash

type signature_t = Valid | Invalid
type filepath_t = string
type checksum_t = string
type checksum_record_t = { filepath: filepath_t; checksum: checksum_t }
type checksum_file_t = int * checksum_record_t list

(*
let entries_of_dir_handle dir_handle =
  let entries = ref [] in
  let () = try
    while true do
      entries := readdir dir_handle :: !entries
    done
  with
     (* TODO getting "unbound constructor" error  with End_of_directory *)
    Unix_error (_ , _, _) ->  ()
    | _ -> ()
  in List.fast_sort compare !entries
*)
let string_ends_with s1 s2 =
  let substring = String.sub s1
    ((String.length s1) - (String.length s2))
    (String.length s2)
  in 0 == compare substring s2

let strip_from_end s e =
  let ret = ref s
  and elen = String.length e
  in let () =
  while string_ends_with !ret e do
    ret := String.sub
    !ret 0 ((String.length !ret) - elen)
  done
  in !ret

let strip_sha256_whitespace hash =
  let hlen = Buffer.length hash in
  let ret = String.create (hlen / 3 * 2) in
  let ()=
    for i = 0 to hlen / 3 - 2 do
       let offset = i*3 + 
         if i > 15 then 2 else 1
       in
       blit hash offset ret (i*2) 2
    done
  in ret
  
let rec map_dir f path =
  (* this does a lot of string concatenation
     and dir accessing - not optimized *)
  let rec _map_dir f path=
    let success = ref true in
    let path = (strip_from_end path dir_sep) ^ dir_sep in (* should only be used on user input *)
    (* call ?dir_callback path *)
    let _ = try 
(*    let dirarr = entries_of_dir_handle dir in TODO for use with Unix.openat *)
      let dirarr = readdir path in
      (* if ~sort then *)
      Array.fast_sort compare dirarr;
      for i = 0 to Array.length dirarr -1 do
        if !success then
        let dirent = path ^ dirarr.(i) in
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
     | _ -> f path (* call ~file_callback *)
  end else
    false (* map not succsssful*)

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

let path_and_hash file =
  let ret = 
  { filepath = file;
    checksum = sha256sum file }
  in ret

let print_hash file=
  let () =
  let checksum = path_and_hash file in
  let acc = String.concat " " [
    checksum.filepath;
    checksum.checksum
    ] in
    print_endline acc 
  in true

let () =
(* Sys.set_signal Sys.sigusr1 print_progress *)

(* map_dir should return an option of errors with Some having a description of what went wrong *)
  if map_dir print_hash argv.(1) then
    print_endline "success"
  else
    print_endline "failure"
    (* should save a descripyion of what went wrong - exceptions? *)
