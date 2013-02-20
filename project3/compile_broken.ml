(* Compile Cish AST to MIPS AST *)
open Mips
open Ast

exception IMPLEMENT_ME;;
exception BadProgram;;
let error s = (print_string ("Error: "^ s); raise BadProgram);;

let rec zip lst1 lst2 = 
  match lst1,lst2 with 
      [],_ -> []
    | _, []-> []
    | (x::xs),(y::ys) -> (x,y) :: (zip xs ys)
;;

let var_prefix = "var_";;

type result = { code : Mips.inst list;
                data : Mips.label list }
;;

type funenv = (string * int) list;;

let funenv_insert (var : string) (env : funenv) : funenv =
  let offset = List.length env + 3 in
    (var, offset) :: env
;;
  
let rec funenv_lookup (var : string) (env : funenv) : int32 =
  let rec lookup v e = 
    match e with
        [] -> error("variable "^var^" was not found in environment")
      | hd :: tl -> 
          let n, offset = hd in
            if n = v then offset else lookup v tl
  in
  let res = lookup var env in
    Int32.of_int (res * 4)
;;

let funenv_offsets (env : funenv) : int list =
  List.map (fun a -> let k,v = a in v) env
;;

let funenv_size (env : funenv) : int =
  (List.length env + 2) * 4
;;

let rec funenv_to_string (env : funenv) : string =
  match env with
      [] -> ""
    | hd :: tl ->
        let v, offset = hd in
        let str = Printf.sprintf "(%s, %d)\n" v offset in
          str^(funenv_to_string tl)
;;

let funenv_table = Hashtbl.create 10;;

let funenv_table_print tbl : unit =
  Hashtbl.iter (fun k v -> 
                  Printf.printf "%s:\n%s\n" (k^"'s env") (funenv_to_string v))
    tbl
;;

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
let rec collect_fun_vars (f : Ast.funcsig) : unit =
  let rec collect_stmt_vars ((s : Ast.rstmt), (pos : int)) (env : funenv) : funenv =
    match s with
      | Ast.Exp exp -> env
      | Ast.Seq(s1,s2) -> collect_stmt_vars s1 (collect_stmt_vars s2 env)
      | Ast.If(exp,s1,s2) -> collect_stmt_vars s1 (collect_stmt_vars s2 env)
      | Ast.While(exp,s1) -> collect_stmt_vars s1 env
      | Ast.For(exp1,exp2,exp3,s1) -> collect_stmt_vars s1 env
      | Ast.Return exp -> env
      | Ast.Let(v,exp,s) -> collect_stmt_vars s (funenv_insert v env)
  in
  let rec collect_fun_args (args : var list) (env : funenv) : funenv =
    match args with
        [] -> env
      | hd :: tl -> collect_fun_args tl (funenv_insert hd env)
  in
  let f_name = f.name in
    if (Hashtbl.mem funenv_table f_name) then
      error("Function "^f_name^" has already been defined")
    else
      let f_args = f.args in
      let f_body = f.body in
      let f_env = collect_stmt_vars f_body (collect_fun_args f_args []) in
        Hashtbl.add funenv_table f_name (List.rev_append f_env []);
;;

(* compiles a fish statement *)
let rec compile_exp (i : exp) (frame : funenv) : inst list =

  let exp_prec (e1 : exp) (e2 : exp) : inst list =
    let t = new_temp() in
      (compile_exp e1 frame) @ [La(R3, t); Sw(R2, R3, Word32.zero)]
      @(compile_exp e2 frame) @ [La(R3, t); Lw(R3, R3, Word32.zero)]
  in

  let rec fun_prologue (exps : (exp * int) list) (env : funenv) : inst list =
    let get_arg_inst (arg : exp) (i : int) : inst list =
      let arg_inst = compile_exp arg env in
      let off = Int32.of_int ((funenv_size env) - (i*4)) in
        arg_inst @ [Sw(R2, R29, off)]
    in
    let partial_prologue =
      match exps with
          [] -> []
        | hd :: tl ->
            let e,offset = hd in
              (get_arg_inst e offset) @ fun_prologue tl env
    in
    let stack_size = funenv_size env in
    let return_offset = Int32.of_int (stack_size - 4) in
    let fp_offset = Int32.of_int (stack_size - 8) in
      [Mips.Add(R29, R29, Immed (Int32.neg (Int32.of_int stack_size)));
       Mips.Sw(R31, R29, return_offset); (* store return address *)
       Mips.Sw(R30, R29, fp_offset); (* store frame pointer *)
       Mips.Add(R30, R31, Immed return_offset); (* calculate new frame pointer *)
      ] @ partial_prologue
  in

  let rec fun_epilogue (env : funenv) : inst list =
    let stack_size = funenv_size env in
    let return_offset = Int32.of_int (stack_size - 4) in
    let fp_offset = Int32.of_int (stack_size - 8) in
    [Mips.Lw(R31, R29, return_offset); (* restore return address*)
     Mips.Lw(R30, R29, fp_offset); (* restore frame pointer *)
     Mips.Add(R29, R29, Immed (Int32.of_int stack_size));
     Mips.Jr(R31);] (* return to callee *)
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
           | Lt    -> [Mips.Slt(R2, R3, Reg R2)]
           | Lte   -> [Mips.Sle(R2, R3, R2)]
           | Gt    -> [Mips.Sgt(R2, R3, R2)]
           | Gte   -> [Mips.Sge(R2, R3, R2)])
    | Not (e1) ->
        (compile_exp e1 frame) @ [Mips.Nor(R2, R2, R0)]
    | And (i1, i2) -> 
        (exp_prec i1 i2) @ [Mips.And(R2, R2, Reg R3)]
    | Or  (i1, i2) ->
        (exp_prec i1 i2) @ [Mips.Or(R2, R2, Reg R3)]
    | Assign(x,e) -> (compile_exp e frame) @ [La(R3,(var_prefix^x)); Sw(R2,R3,Word32.zero)]
    | Call(v, exps) -> 
        let v_env = Hashtbl.find funenv_table v in
        let exp_offsets = zip exps (funenv_offsets v_env) in
          (fun_prologue exp_offsets v_env) @ [Jal v] @ (fun_epilogue v_env)
;;

(* compiles a Fish statement down to a list of MIPS instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in R2 and then doing a Jr R31.
 *)
let rec compile_stmt ((s,_):Ast.stmt) (frame : funenv) : inst list = 
  match s with
    | Exp e ->
        compile_exp e frame
    | Seq(s1,s2) ->
        (compile_stmt s1 frame) @ (compile_stmt s2 frame)
    | If(e,s1,s2) ->
        (let else_l = new_label() in
         let end_l = new_label() in
           (compile_exp e frame) @ [Mips.Beq(R2, R0, else_l)] @
           (compile_stmt s1 frame) @ [Mips.J end_l; Mips.Label else_l] @
           (compile_stmt s2 frame) @ [Mips.Label end_l])
    | While(e,s) ->
        (let test_l = new_label() in
         let top_l = new_label() in
           [Mips.J test_l; Mips.Label top_l] @
           (compile_stmt s frame) @
           [Mips.Label test_l] @
           (compile_exp e frame) @
           [Mips.Bne(R2,R0,top_l)])
    | For(e1,e2,e3,s) ->
        compile_stmt (Seq((Exp e1, 0),(While(e2,(Seq(s,(Exp e3, 0)), 0)), 0) ), 0) frame
    | Return e -> 
        compile_exp e frame 
        (* Store result in temporary register R8 and exit the program *)
        (* [Add(R8, R2, Reg R0); Li(R2, 10l); Syscall] *)
    | Let(v,e,s) ->
        let offset = funenv_lookup v frame in
          (compile_exp e frame) @ [Mips.Sw(R2, R29, offset)] @
          (compile_stmt s frame)
;;


let rec compile_func (f : Ast.func) : inst list =
  let f_sig = Ast.get_funcsig f in
  let f_env = Hashtbl.find funenv_table f_sig.name in
    [Label f_sig.name] @ (compile_stmt f_sig.body f_env)
;;

let compile (p : Ast.program) : result =
  let rec init_fun_envs (prog : Ast.program) : unit =
    match prog with
        [] -> ()
      | hd :: tl -> 
          let f_sig = Ast.get_funcsig hd in
            collect_fun_vars f_sig;
            init_fun_envs tl
  in

  let rec compile_prog (prog : Ast.program) : inst list =
    match prog with
        [] -> []
      | hd :: tl -> compile_func hd @ compile_prog tl
  in
    let _ = reset() in
    let _ = init_fun_envs p in
      { code = compile_prog p; data = VarSet.elements (!variables) }
;;

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

