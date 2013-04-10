open Cfg_ast
open Cfg
open Interfere_graph

let b = Var "b"
let c = Var "c"
let d = Var "d"
let e = Var "e"
let f = Var "f"
let g = Var "g"
let h = Var "h"
let j = Var "j"
let k = Var "k"
let m = Var "m"
let res = Var "result"

let main_block : block =
  Label "L0"::
  Load(g,j,12)::
  Arith(h,k,Minus,Int 1)::
  Arith(f,g,Times,h)::
  Load(e,j,8)::
  Load(m,j,16)::
  Load(b,f,0)::
  Arith(c,e,Plus,Int 8)::
  Move(d,c)::
  Arith(k,m,Plus,Int 4)::
  Move(j,b)::
  (* Arith(res,j,Plus,k):: *)
  (* Move(Reg(Mips.R2), res):: *)
  Return::[]

let if_block_1 : block =
  Label "L1"::Return::[]

let if_block_2 : block =
  Label "L2"::Return::[]

let _ =
  let func : func = [main_block] in
  (* let func_str = Cfg_ast.prog2string [func] in *)
  let cfg = build_cfg func in
    print_graph (build_interfere_graph cfg);

