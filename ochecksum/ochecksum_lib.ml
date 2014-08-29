open Sys
open Nocrypto.Hash
open Cstruct
open ChecksumRecurse

let strip_sha256_whitespace hash =
  let hlen = Buffer.length hash in
  let ret = String.create (hlen / 3 * 2) in
  let ()=
    for i = 0 to hlen / 3 - 2 do
       let offset = i*3 + 
         if i > 15 then 2 else 1
       in
       Buffer.blit hash offset ret (i*2) 2
    done
  in ret

let sha256sum ~path : checksum_t option =
  let mmap_file = 
    Unix_cstruct.of_fd
      Unix.(openfile path [O_RDONLY] 0)
  and hexbuf = Buffer.create (32*3) in
  let () = 
    Cstruct.hexdump_to_buffer
      hexbuf
      (SHA256.digest mmap_file)
  in
    Some (strip_sha256_whitespace hexbuf : string :> checksum_t)
(* in Core_string.filter (function '\n'|' '->false|true) -- im too stupid to manage installation of Core *)

let ochecksum ~(path:string) : string option=
(* Sys.set_signal Sys.sigusr1 print_progress *)
  checksumrecurse
   ~sort:true
   ~checksum_callback:sha256sum
   ~path
   ()

