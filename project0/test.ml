open OUnit
open Util_test
open Mach_test

let suite = 
"suite">:::
 ["hex_to_bin_test">:: hex_to_bin_test;
  "int_to_bin_test">:: int_to_bin_test;
  "mips_to_mach_test">:: mips_to_mach_test;
  "psuedo_to_mach_test">:: psuedo_to_mach_test]
;;

let _ =
  run_test_tt_main suite
