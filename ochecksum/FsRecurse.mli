val fsrecurse :
  ?file_callback:(path:string -> string option)
-> ?dir_callback:(path:string -> string option)
-> ?ignore:string list
-> path:string
-> sort:bool
(* returns: *)
-> string option

