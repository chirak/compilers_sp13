open OUnit
open Mips_ast
open Mips_sim


let li_one  = Li (R16, 25l);;
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


let mips_to_mach_test () =
  let actual = assemble mips in
    assert_equal actual machine_code;
;;

let psuedo_to_mach_test () = 
  let li_mach_one = assemble (li_one :: []) in
  let li_mach_two = assemble (li_two :: []) in
    assert_equal li_mach_one (lui_mach_one :: ori_mach_one :: []);
    assert_equal li_mach_two (lui_mach_two :: ori_mach_two :: []);
;;

