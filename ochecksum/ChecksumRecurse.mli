type filepath_t = string
type checksum_t = string
type checksum_record_t = {
  filepath : filepath_t;
  filelength : int64;
  checksum : checksum_t;
}
type checksum_file_t = checksum_record_t list

val checksumrecurse :
   checksum_callback:(path:string -> checksum_t option)
-> sort:bool
-> path:string
-> unit
(* returns: *)
-> string option
