val recurse : path:string -> sort:bool
  -> ?ignore:string list
  -> ?dir_callback:( path:string -> option )
  -> ?file_callback:( path:string -> option)
  -> option

