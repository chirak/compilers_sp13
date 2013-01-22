open OUnit
open Mips_ast
open Practice

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

let assert_machine_code i expected =
  let actual = mips_to_mach i in
    assert_equal (String.length actual) 32;
    assert_equal actual expected;
;;

let add = Add (R1, R10, R22);;

let mips_to_mach_test () =
  assert_equal (mips_to_mach add1) "0b000000010101011000001100000";
;;

let suite = 
"suite">:::
 ["hex_to_bin_test">:: hex_to_bin_test;
  "int_to_bin_test">:: int_to_bin_test]
;;

let _ =
  run_test_tt_main suite
