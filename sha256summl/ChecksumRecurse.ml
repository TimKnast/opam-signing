open FilesystemRecurse

type signature_t = Valid | Invalid
type filepath_t = string
type checksum_t = string
type checksum_record_t = {
  filepath: filepath_t;
  filelength: int64; (*uint64*)
  checksum: checksum_t
 }
type checksum_file_t = checksum_record_t list

let path_and_hash (file:string)
  (checksum_callback: string -> checksum_t)
  =
  let ret = 
  { filepath = file;
    checksum = checksum_callback file }
  in ret

let print_hash ~checksum_callback file =
  let () =
  let checksum = path_and_hash file checksum_callback in
  let acc = String.concat " " [
    checksum.filepath;
    checksum.checksum
    ] in
    print_endline acc 
  in Nothing




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

let recurse ~file_callback ~checksum_callback
  (~sort:bool) ~path : option =
  FilesystemRecurse ~path ~sort ~file_callback:(print_and_hash ~checksum_callback)

