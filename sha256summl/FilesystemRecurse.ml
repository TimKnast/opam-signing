open Buffer
open Sys
open Filename
(* open Unix TODO for proper openat *)
open Unix_cstruct

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

let rec map_dir f path =
  let rec _map_dir f path=
    let success = ref (dir_callback dir) in
    match !success with Nothing ->
    let _ = try 
      let dirarr = readdir path in
      if sort then begin
        Array.fast_sort compare dirarr
      end;
      for i = 0 to Array.length dirarr -1 do
        match !success with
          | Nothing -> 
            let dirent = path ^ dirarr.(i) in
            success := map_dir f dirent
          | _ -> () done
      done 
    with
     error -> success := Something error 
    in !success done
  in
  if file_exists path then begin
  (*should check its not a socket or symlink etc? *)
    match basename path with
     | "" (* ends in / *)
     | _ when is_directory path
       -> _map_dir f path
     | _ -> file_callback path
  end else
    Something "No such file: " ^ path

let fsrecurse
  (~path:string)
  (~sort:bool)
  (?ignore: string list)
  (?dir_callback: string -> option )
  (?file_callback: string -> option )
  =
  let path = (strip_from_end path dir_sep) ^ dir_sep in
  map_dir ~path ~sort ("."::".."::ignore)
    ?dir_callback ?file_callback

