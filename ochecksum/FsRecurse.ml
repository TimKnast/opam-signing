open Buffer
open Sys
open Filename

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

let default_file_callback ~(path:string) : string option =
  None

let default_dir_callback ~(path:string) : string option =
  None

let rec map_dir
  ~file_callback
  ~dir_callback
  ~ignore
  ~(path:string)
  ~(sort:bool) : string option =
  let rec _map_dir 
~file_callback ~dir_callback ~ignore ~path ~sort
 : string option=
    let success = ref (dir_callback ~path) in
    let _ = try 
      let dirarr = readdir path in
      if sort then begin
        Array.fast_sort compare dirarr
      end;
      for i = 0 to Array.length dirarr -1 do
        match !success with
          | None -> 
            let dirent = path ^ dir_sep ^ dirarr.(i) in
            success := map_dir ~file_callback ~dir_callback ~ignore ~path:dirent ~sort
          | _ -> () done
    with
     error -> success := Some "error in FsRecurse._map_dir"
    in !success 
  in
  if file_exists path then begin
  (*should check its not a socket or symlink etc? *)
    match path with
     | _ when is_directory path
       -> _map_dir ~file_callback ~dir_callback ~ignore ~path ~sort
     | _ -> file_callback ~path
  end else
    Some ("No such file: " ^ path)


let fsrecurse
  ?(file_callback=default_file_callback)
  ?(dir_callback=default_dir_callback)
  ?(ignore=[])
  ~(path:string)
  ~(sort:bool)
  ()
  : string option
  =
  let path = (strip_from_end path dir_sep) in
  map_dir ~path ~sort
    ~ignore:("."::".."::ignore)
    ~dir_callback ~file_callback

