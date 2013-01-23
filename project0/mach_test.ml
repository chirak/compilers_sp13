open OUnit
open Mips_ast
open Mach

let empty_list = []
let list_a = [1;2;3]

let hex_to_bin_test () =
  assert_equal (hex_to_bin '0') "0000";
  assert_equal (hex_to_bin '1') "0001";
  assert_equal (hex_to_bin '2') "0010";
  assert_equal (hex_to_bin '3') "0011";
  assert_equal (hex_to_bin '4') "0100";
  assert_equal (hex_to_bin '5') "0101";
  assert_equal (hex_to_bin '6') "0110";
  assert_equal (hex_to_bin '7') "0111";
  assert_equal (hex_to_bin '8') "1000";
  assert_equal (hex_to_bin '9') "1001";
  assert_equal (hex_to_bin 'A') "1010";
  assert_equal (hex_to_bin 'B') "1011";
  assert_equal (hex_to_bin 'C') "1100";
  assert_equal (hex_to_bin 'D') "1101";
  assert_equal (hex_to_bin 'E') "1110";
  assert_equal (hex_to_bin 'F') "1111";
  assert_equal (hex_to_bin 'a') "1010";
  assert_equal (hex_to_bin 'b') "1011";
  assert_equal (hex_to_bin 'c') "1100";
  assert_equal (hex_to_bin 'd') "1101";
  assert_equal (hex_to_bin 'e') "1110";
  assert_equal (hex_to_bin 'f') "1111";
;;

let int_to_bin_test () =
  assert_equal (int_to_bin (5, 0)) "00000";
  assert_equal (int_to_bin (5, 5)) "00101";
  assert_equal (int_to_bin (5, 31)) "11111";
  assert_equal (int_to_bin (5, 40)) "01000";

  assert_equal (int_to_bin (6, 0)) "000000";
  assert_equal (int_to_bin (6, 11)) "001011";
  assert_equal (int_to_bin (6, 34)) "100010";
  assert_equal (int_to_bin (6, 63)) "111111";
;;

let li_one  = Li (R16, (Int32.of_int 25));;
let lui_one = Lui (R16, (Int32.of_int 0));;
let ori_one = Ori (R16, R16, (Int32.of_int 25));;
let li_two  = Li (R17, (Int32.of_int 134075));;
let lui_two = Lui (R17, (Int32.of_int 2));;
let ori_two = Ori (R17, R17, (Int32.of_int 3003));;
let add = Add (R1, R10, R22);;

let mips = lui_one ::
           ori_one ::
           lui_two ::
           ori_two ::
           add :: []
;;

let lui_mach_one = "00111100000100000000000000000000";;
let ori_mach_one = "00110110000100000000000000011001";;
let lui_mach_two = "00111100000100010000000000000010";;
let ori_mach_two = "00110110001100010000101110111011";;
let add_mach     = "00000001010101100000100000100000";;

let machine_code = lui_mach_one ::
                   ori_mach_one ::
                   lui_mach_two ::
                   ori_mach_two ::
                   add_mach :: []
;;

(* 100000101110111011 *)

let mips_to_mach_test () =
  let actual = assem mips in
    assert_equal actual machine_code;

  (* let add_mach =  "00000001010101100000100000100000" in *)
  (*   assert_equal (mips_to_mach add)  add_mach; *)

  (* let lui_mach_one = "00111100000100000000000000000000" in *)
  (* let ori_mach_one = "00110110000100000000000000011001" in *)
  (*   assert_equal (mips_to_mach lui_one) lui_mach_one; *)
  (*   assert_equal (mips_to_mach ori_one) ori_mach_one; *)
  (*   assert_equal (mips_to_mach li_one) (lui_mach_one^ori_mach_one); *)

  (* let lui_mach_two = "00111100000100010000000000000010" in *)
  (* let ori_mach_two = "00110110001100010000101110111011" in *)
  (*   assert_equal (mips_to_mach lui_two) lui_mach_two; *)
  (*   assert_equal (mips_to_mach ori_two) ori_mach_two; *)
  (*   assert_equal (mips_to_mach li_two) (lui_mach_two^ori_mach_two); *)
;;

let psuedo_to_mach_test () = 
  let li_mach_one = assem (li_one :: []) in
  let li_mach_two = assem (li_two :: []) in
    assert_equal li_mach_one (lui_mach_one :: ori_mach_one :: []);
    assert_equal li_mach_two (lui_mach_two :: ori_mach_two :: []);
;;

let suite = 
"suite">:::
 ["hex_to_bin_test">:: hex_to_bin_test;
  "int_to_bin_test">:: int_to_bin_test;
  "mips_to_mach_test">:: mips_to_mach_test;
  "psuedo_to_mach_test">:: psuedo_to_mach_test]
;;

let _ =
  run_test_tt_main suite
