(* Simulates a four byte word *)
open Utils

type word = { w :  int32 };;

let mk_word (i : int32) : word =
  { w = Int32.logand 0xFFFFFFFFl i }
;;

let str_to_word (prefix : string) (str : string) : word =
  { w = (Int32.of_string (prefix^str)) }
;;

let get_hex_str (w : word) : string =
  Printf.sprintf "%lx" w.w
;;


(* Functions for MIPS instructions *)
let get_range (w : word) (start : int) (len : int) : int32 =
  let bin_word = int32_to_bin (32, w.w) in
    bin_to_int32 (String.sub bin_word start len)
;;

let get_opcode (w : word) : int32 = get_range w 0 6;;
let get_func   (w : word) : int32 = get_range w 26 6;;
let get_rs     (w : word) : int32 = get_range w 6 5;;
let get_rt     (w : word) : int32 = get_range w 11 5;;
let get_rd     (w : word) : int32 = get_range w 16 5;;
let get_offset (w : word) : int32 = get_range w 16 16;;
let get_rd     (w : word) : int32 = get_range w 6 26;;


