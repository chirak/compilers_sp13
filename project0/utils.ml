(* Holds operations for hexadecimal and binary strings *)

(* Returns list of chars for the given string *)
let explode_str s : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

(* Turns a single hexadecimal char into a 4 bit binary string *)
let hex_to_bin (c : char) : string =
    match c with
        '0' -> "0000"
      | '1' -> "0001"
      | '2' -> "0010"
      | '3' -> "0011"
      | '4' -> "0100"
      | '5' -> "0101"
      | '6' -> "0110"
      | '7' -> "0111"
      | '8' -> "1000"
      | '9' -> "1001"
      | ('A'|'a') -> "1010"
      | ('B'|'b') -> "1011"
      | ('C'|'c') -> "1100"
      | ('D'|'d') -> "1101"
      | ('E'|'e') -> "1110"
      | ('F'|'f') -> "1111"
      | ch -> raise (Invalid_argument (Printf.sprintf "Invalid char, %c, given" ch))
;;

let hex_to_int32 (str : string) : int32 =
  Int32.of_string ("0x"^str)
;;

let int32_to_hex (i : int32) : string =
  Printf.sprintf "%lx" i
;;

(* Creates a n-bit binary string out of the given int32. 'n' is specified by
 * first element of the tuple *)
let int32_to_bin (i : int * int32) : string =
  let rec get_binary_str lst accum =
    match lst with
        [] -> accum
      | hd :: tl -> get_binary_str tl (accum^(hex_to_bin hd))
  in

  (* Removes leading zeros from binary string *)
  let trim bin_str str_len size =
      String.sub bin_str (str_len - size) size 
  in

  (* Pads binary string with zeros to match the given bit size *)
  let rec pad bin_str diff =
    match diff with
        0 -> bin_str
      | k -> pad ("0"^bin_str) (diff - 1)
  in

  let size, int_value = i in
  let hex_chars = explode_str (int32_to_hex int_value) in
  let bin_str = get_binary_str hex_chars "" in
  let bin_str_len = String.length bin_str in
    if size < bin_str_len then
      trim bin_str bin_str_len size
    else
      pad bin_str (size - bin_str_len)
;;

(* Creates a n-bit binary string out of the given int. 'n' is specified by
 * first element of the tuple *)
let int_to_bin (i : int * int) : string =
  let size, value = i in
  let j = size, Int32.of_int value in
    int32_to_bin j
;;

let bin_to_int32 (str : string) : int32 =
  Int32.of_string ("0b"^str)
;;

