(* Compile Cish AST to MIPS AST *)
open Mips

exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

let rec compile (p:Ast.program) : result =
    raise IMPLEMENT_ME

let result2string (res:result) : string = 
  let code = res.code in
  let data = res.data in
  let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
  let var2decl x = x ^ ":\t.word 0\n" in
  "\t.text\n" ^
    "\t.align\t2\n" ^
    "\t.globl main\n" ^
    (String.concat "" strs) ^
    "\n\n" ^
    "\t.data\n" ^
    "\t.align 0\n"^
    (String.concat "" (List.map var2decl data)) ^
    "\n"
