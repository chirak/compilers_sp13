(* Compile Fish AST to MIPS AST *)
open Mips
open Ast

exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }
;;

let var_prefix = "var_";;

(* generate fresh labels *)
let label_counter = ref 0;;
let new_int() = (label_counter := (!label_counter) + 1; !label_counter);;
let new_label() = "L" ^ (string_of_int (new_int()));;

(* sets of variables -- Ocaml Set and Set.S *)
module VarSet = Set.Make(struct
                           type t = Ast.var
                           let compare = String.compare
                         end)
;;

(* a table of variables that we need for the code segment *)
let variables : VarSet.t ref = ref (VarSet.empty);;

(* generate a fresh temporary variable and store it in the variables set. *)
let rec new_temp() = 
    let t = "T" ^ (string_of_int (new_int())) in
    (* make sure we don't already have a variable with the same name! *)
    if VarSet.mem t (!variables) then new_temp()
    else (variables := VarSet.add t (!variables); t)
;;

(* reset internal state *)
let reset() = (label_counter := 0; variables := VarSet.empty);;

(* find all of the variables in a program and add them to
 * the set variables *)
let rec collect_vars (p : Ast.program) : unit = 

  let rec collect_vars_exp ((e : Ast.rexp), (pos : int)) =
    match e with
        Ast.Assign (x, e1) -> 
          variables := VarSet.add (var_prefix^x) !variables;
          collect_vars_exp e1
      | _ -> ()
  in

  let s, pos = p in

    match s with
        Ast.Exp e -> collect_vars_exp e
      | Ast.Seq(s1,s2) -> (collect_vars s1; collect_vars s2)
      | Ast.If(e,s1,s2) -> (collect_vars s1; collect_vars s2)
      | Ast.While(e,s1) -> collect_vars s1;
      | Ast.For(e1,e2,e3,s1) -> (collect_vars_exp e1; collect_vars s1)
      | Ast.Return e -> ()
;;

(* Compiles a Fish statement *)
let rec compile_exp (i : exp) : inst list =

  let exp_prec (e1 : exp) (e2 : exp) : inst list =
    let t = new_temp() in
      (compile_exp e1) @ [La(R3, t); Sw(R2, R3, Word32.zero)]
      @(compile_exp e2) @ [La(R3, t); Lw(R3, R3, Word32.zero)]
  in

  let e, pos = i in
  match e with
    | Int j -> 
        [Li(R2,  Word32.fromInt j)]
    | Var x -> 
        [La(R2,(var_prefix^x)); Lw(R2, R2, Word32.zero)]
    | Binop(i1, b, i2) ->
        (exp_prec i1 i2)
       @(match b with
             Plus  -> [Mips.Add(R2, R2, Reg R3)]
           | Minus -> [Mips.Sub(R2, R3, R2)]
           | Times -> [Mips.Mul(R2, R2, R3)]
           | Div   -> [Mips.Div(R2, R3, R2)]
           | Eq    -> [Mips.Seq(R2, R2, R3)]
           | Neq   -> [Mips.Sne(R2, R2, R3)]
           | Lt    -> [Mips.Slt(R2, R3, R2)]
           | Lte   -> [Mips.Sle(R2, R3, R2)]
           | Gt    -> [Mips.Sgt(R2, R3, R2)]
           | Gte   -> [Mips.Sge(R2, R3, R2)])
    | Not (e1) ->
        (compile_exp e1) @ [Mips.Nor(R2, R2, R0)]
    | And (i1, i2) -> 
        (exp_prec i1 i2) @ [Mips.And(R2, R2, Reg R3)]
    | Or  (i1, i2) ->
        (exp_prec i1 i2) @ [Mips.Or(R2, R2, Reg R3)]
    | Assign(x,e) -> (compile_exp e) @ [La(R3,(var_prefix^x)); Sw(R2,R3,Word32.zero)]
;;

(* compiles a Fish statement down to a list of MIPS instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in R2 and then doing a Jr R31.
 *)
let rec compile_stmt ((s,_):Ast.stmt) : inst list = 
  match s with
    | Exp e ->
        compile_exp e
    | Seq(s1,s2) ->
        (compile_stmt s1) @ (compile_stmt s2)
    | If(e,s1,s2) ->
        (let else_l = new_label() in
         let end_l = new_label() in
           (compile_exp e) @ [Mips.Beq(R2, R0, else_l)] @
           (compile_stmt s1) @ [Mips.J end_l; Mips.Label else_l] @
           (compile_stmt s2) @ [Mips.Label end_l])
    | While(e,s) ->
        (let test_l = new_label() in
         let top_l = new_label() in
           [Mips.J test_l; Mips.Label top_l] @
           (compile_stmt s) @
           [Mips.Label test_l] @
           (compile_exp e) @
           [Mips.Bne(R2,R0,top_l)])
    | For(e1,e2,e3,s) ->
        compile_stmt(Seq((Exp e1, 0),(While(e2,(Seq(s,(Exp e3, 0)), 0)), 0) ), 0)
    | Return e -> 
        (* Store result in temporary register R8 and exit the program *)
        compile_exp e @ [Add(R8, R2, Reg R0); Li(R2, 10l); Syscall]
;;


(* compiles Fish AST down to MIPS instructions and a list of global vars *)
let compile (p : Ast.program) : result = 
    let _ = reset() in
    let _ = collect_vars(p) in
    let insts = (Label "main") :: (compile_stmt p) in
    { code = insts; data = VarSet.elements (!variables) }
;;

(* converts the output of the compiler to a big string which can be 
 * dumped into a file, assembled, and run within the SPIM simulator
 * (hopefully). *)
let result2string ({code;data}:result) : string = 
    let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
    let var2decl x = x ^ ":\t.word 0\n" in
    "\t.text\n" ^
    "\t.align\t2\n" ^
    "\t.globl main\n" ^
    (String.concat "" strs) ^
    "\n\n" ^
    "\t.data\n" ^
    "\t.align 0\n"^
    (String.concat "" (List.map var2decl data))
;;

