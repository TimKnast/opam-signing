val fsrecurse :
  ?file_callback:(path:string -> string option)
-> ?dir_callback:(path:string -> string option)
-> ?ignore:string list
-> path:string
-> sort:bool
-> unit (* #wtfocaml *)
(* returns: *)
-> string option

