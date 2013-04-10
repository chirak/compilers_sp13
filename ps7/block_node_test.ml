(*open Cfg_ast
open Cfg
(* 
 * Label L1
 * t2 = t1
 * t3 = t2 * 1
 * if t2 = t3 then L2 else L3
 *)

let t1 = Var "t1"
let t2 = Var "t2"
let t3 = Var "t3"
let t4 = Var "t4"

let i1 = Label("L1")
let i_set1 =
  { i = i1;
    igen_set  = empty_set;
    ikill_set = empty_set }

let i2 = Move(t2, t1)
let i_set2 =
  { i = i1;
    igen_set = vs_add_all [t2];
    ikill_set = vs_add_all [t1]}

let i3 = Arith(Var("t3"), Var("t2"), Times, Int 1)
let i_set3 =
  { i = i1;
    igen_set  = vs_add_all [t2];
    ikill_set = vs_add_all [t3]}

let i4 = If(Var "t2", Eq, Var "t3", "L2", "L4")
let i_set4 =
  { i = i1;
    igen_set  = empty_set;
    ikill_set = empty_set }

let return = Return

let _ =
  let expected = [i_set1; i_set2; i_set3; i_set4; return;] in
  let actual = List.Map generate_inst_set [i1;i2;i3;i4;return] in
  let pass =

*)