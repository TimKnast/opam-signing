open FsRecurse
open Buffer
open Int64
open Unix.LargeFile

type sorted_t = Sorted | Unsortd
type signature_t = Valid | Invalid
type filepath_t = string
type checksum_t = string
type checksum_record_t = {
  filepath: filepath_t;
  filelength: int64; (*uint64*)
  checksum: checksum_t
 }
type checksum_file_t = checksum_record_t list

let path_and_hash ~(path:string)
  ~(checksum_callback:
      path:string -> checksum_t option)
  : checksum_record_t option =
  let stat = Unix.LargeFile.stat path in
  match stat.st_kind with
  | Unix.S_REG -> Some {
      filepath = path;
      filelength = stat.st_size; 
      checksum = match checksum_callback ~path with
        | None -> "ERROR"
        | Some s -> s
    }
(*todo handle checksum error*)
  | _ -> None

let print_hash ~checksum_callback ~(path:string) : string option =
  let checksum = path_and_hash ~path ~checksum_callback in
  match checksum with
  | None -> Some ("error: not a regular file: "^path)
  | Some c -> let acc = String.concat " " [
      c.filepath;
      Int64.to_string c.filelength;
      c.checksum;
      ] in
    print_endline acc ;
    None

let checksumrecurse ~checksum_callback
  ~(sort:bool) ~(path:string) (): string option =
  fsrecurse ~file_callback:(print_hash ~checksum_callback) ~path ~sort ()

