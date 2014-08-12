open Sys
open Filename
open Nocrypto

let rec map_dir f path =
  (* this does a lot of string concatenation
     and dir accessing - not optimized *)
  let rec _map_dir f path =
    let success = ref true in
    let _ = try 
    let dirarr = readdir path in
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

let () =
(* Sys.set_signal Sys.sigusr1 print_progress *)

  let fn = (fun x -> print_endline x; true) in
(* this would be a function that checks passed pathname, gets the sha256 checksum and compares *) 

  if map_dir fn argv.(1) then
    print_endline "yes" 
  else
    print_endline "failure"
    (* should save a descripyion of what went wrong - exceptions? *)
