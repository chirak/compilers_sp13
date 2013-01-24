(* Simulates a byte *)

open Utils

type byte = { b : int32 };;

let mk_byte (i : int32) : byte = { b = Int32.logand 0x000000FFl i };;
let b2i32 (b : byte) : int32 = b.b;;
let bin2b (bin : string) : byte =
  let int_val = Int32.of_string ("0b"^bin) in
    mk_byte int_val
;;

let b2bin (b : byte) : string =
  int32_to_bin(8, b.b)
;;
