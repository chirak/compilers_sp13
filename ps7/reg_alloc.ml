open Cfg_ast

(*******************************************************************)
(* PS8 TODO:  graph-coloring, coalescing register assignment *)
(* You will need to build a mapping from variables to MIPS registers
 using the ideas behind the graph-coloring register allocation
 heuristics described in class.  This may involve spilling some
 of the variables into memory, so be sure to adjust the prelude
 of the function so that you allocate enough space on the stack
 to store any spilled variables.  The output should be a CFG
 function that doesn't use any variables (except for function
 names.)
 *)

(* Registers used for every function call:
 * Frame Pointer: $31
 * Return Address: $30
 * Callee Saved: $16 - $23
let reg_alloc (f : func) : func = 
  raise Implement_Me

(* Finally, translate the ouptut of reg_alloc to Mips instructions *)
let cfg_to_mips (f : func ) : Mips.inst list = 
  raise Implement_Me

