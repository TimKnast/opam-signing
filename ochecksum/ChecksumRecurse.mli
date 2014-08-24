val recurse : 
  file_callback:( path:string
    -> checksum_record_t -> option )
  -> checksum_callback:( path:string
    -> checksum_record_t )
  -> sort:bool
  -> path:string
  -> option

