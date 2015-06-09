(* Simulates a four byte word *)
open Utils
open Byte

type word = { w :  int32 };;

let mk_word (i : int32) : word =
  { w = Int32.logand 0xFFFFFFFFl i }
;;

let mk_word_from_bin (str : string) : word =
  mk_word (bin_to_int32 str)
;;

let str_to_word (prefix : string) (str : string) : word =
  { w = (Int32.of_string (prefix^str)) }
;;

let get_hex_str (w : word) : string =
  Printf.sprintf "%lx" w.w
;;

let mk_word_from_bytes (bytes : byte list) =
  let rec mk lob accum =
    match lob with
        [] -> accum
      | hd :: tl ->
          let bin_str = b2bin hd in
            mk (List.tl lob) (accum^(bin_str))
  in
    mk_word_from_bin (mk bytes "")
;;

let split_word (w : word) : byte list =
  let bin_str = int32_to_bin (32, w.w) in
  let b_one   = bin2b (String.sub bin_str 0 8) in
  let b_two   = bin2b (String.sub bin_str 8 8) in
  let b_three = bin2b (String.sub bin_str 16 8) in
  let b_four  = bin2b (String.sub bin_str 24 8) in
    b_one :: b_two :: b_three :: b_four :: []
;;

(* Functions for MIPS instructions *)
let get_range (w : word) (start : int) (len : int) : int =
  let bin_word = int32_to_bin (32, w.w) in
    Int32.to_int (bin_to_int32 (String.sub bin_word start len))
;;

let get_opcode  (w : word) : int = get_range w 0 6;;
let get_func    (w : word) : int = get_range w 26 6;;
let get_rs      (w : word) : int = get_range w 6 5;;
let get_rt      (w : word) : int = get_range w 11 5;;
let get_rd      (w : word) : int = get_range w 16 5;;
let get_offset  (w : word) : int = get_range w 16 16;;
let get_immed   (w : word) : int = get_range w 16 16;;
let get_address (w : word) : int = get_range w 6 26;;

