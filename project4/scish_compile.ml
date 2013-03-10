open Cish_ast

exception Unimplemented
exception Error
let error s = (print_string s; print_string "\n"; raise Error);;

(* generate fresh variables *)
let var_counter = ref 0;;
let new_int() = (var_counter := (!var_counter) + 1; !var_counter);;
let new_var prefix = (prefix) ^ (string_of_int (new_int()));;

type env = (string * int) list;;

let rec in_env (v:string) (env:env) : bool =
  match env with
      [] -> false
    | hd::tl -> 
        let v', _ = hd in
          v = v' || in_env v tl
;;

let rec s_lookup (v:string) (env:env) : int =
  match env with
      [] -> error (Printf.sprintf "Variable %s not found in env" v)
    | hd::tl ->
        let v', off = hd in
          if v = v' then off else s_lookup v tl
;;

let rec c_lookup (i:int) (accum:exp) : exp =
  if i <= 0 then
    (Load accum, 0)
  else
    c_lookup (i-1) (Load (Binop(accum, Plus, (Int(4),0)), 0), 0)
;;


let compile_exp (e:Scish_ast.exp) : program = 
  let fun_env : funcsig list ref = ref [] in

  (* Creates a function with the given name, body, args, and vars and puts
     it in the function environment. The function's reference name is returned *)
  let mk_fun (name:string) (body:stmt) (args: string list) (vars: string list) : exp =
    let rec add_vars (v:string list) (accum:stmt) : stmt =
      match v with
          [] -> accum
        | hd::tl -> add_vars tl (Let(hd, (Int(0), 0), accum), 0)
    in
    let body = add_vars vars (Seq(body, (Return((Var("result"), 0)), 0)), 0) in
      fun_env := { name=name; args=args; body=body; pos=0 }::!fun_env;
      (Var(name), 0)
  in
  
  let rec compile' (expr:Scish_ast.exp) (var:string) (env:env) : Cish_ast.stmt =
    let _exp (e:exp) : stmt =
      (Exp e, 0)
    in
    let _let (var:string) (s:stmt) =
      (Let (var, (Int(0), 0), s), 0)
    in
    let rec _seq (stmts:stmt list) : stmt = 
      match stmts with
          [s1;s2] -> (Seq (s1, s2), 0)
        | hd::tl  -> (Seq(hd, (_seq tl)), 0)
        | _ -> error "Sequence requires at least 2 stmts"
    in

    (* Shorthand bindings for CISH exps *)
    let _int (i:int) : exp = (Int i, 0) in
    let _var (v:string) : exp = (Var v, 0) in
    let _binop (v1:exp) op (v2:exp) : exp = (Binop(v1, op, v2), 0) in
    let _assign (v:var) (e:exp) : exp = (Assign (v, e), 0) in
    let _call (f:exp) (args:exp list) : exp = (Call (f, args), 0) in
    let _load (src:exp) : exp = (Load src, 0) in
    let _store (dest:exp) (v:exp) : exp = (Store(dest, v), 0) in
    let _malloc (size:int) : exp = (Malloc (Int(size), 0), 0) in

    let compile_prim op e1 e2 =
      let t1 = new_var "t" in
      let t2 = new_var "t" in
      let v1 = compile' e1 t1 env in
      let v2 = compile' e2 t2 env in
      let res = (Exp (_assign var (_binop (_var t1) op (_var t2))), 0) in
      let s = _seq [v1;v2;res] in
        (_let t1 (_let t2 s))
    in

    let compile_cons fst snd v =
      let t1 = new_var "t" in
      let t2 = new_var "t" in
      let v1 = compile' fst t1 env in
      let v2 = compile' snd t2 env in
      let malloc = _assign v (_malloc 8) in
      let store1 = _store (_var v) (_var t1) in
      let store2 = _store (_binop (_var v) Plus (_int 4)) (_var t2) in
      let s = _seq [v1;v2;(_exp malloc);(_exp store1);(_exp store2)] in
        (_let t1 (_let t2 s))
    in

    let insert v env =
      let offset = (List.length env) + 1 in
        (v,offset)::env
    in

    match expr with
        Scish_ast.Int(i) -> _exp (_assign var (_int i))
      | Scish_ast.Var(v) ->
          if in_env v env then
            let off = (List.length env) - (s_lookup v env) in
            let load = c_lookup off (Var "dynenv", 0) in
              _exp (_assign var load)
          else
            _exp (_assign var (_var v))
      | Scish_ast.PrimApp(op, exprs) ->
          (match (op, exprs) with
               (Scish_ast.Plus,[e1;e2])  -> compile_prim Plus e1 e2
             | (Scish_ast.Minus,[e1;e2]) -> compile_prim Minus e1 e2
             | (Scish_ast.Times,[e1;e2]) -> compile_prim Times e1 e2
             | (Scish_ast.Div,[e1;e2])   -> compile_prim Div e1 e2
             | (Scish_ast.Eq,[e1;e2])    -> compile_prim Eq e1 e2
             | (Scish_ast.Lt,[e1;e2])    -> compile_prim Lt e1 e2
             | (Scish_ast.Cons,[hd;tl])  -> compile_cons hd tl var
             | (Scish_ast.Fst,[e]) ->
                 let s = compile' e var env in 
                 let load = _assign var (_load (_var var)) in
                   _seq [s;(_exp load)]
             | (Scish_ast.Snd,[e]) ->
                 let s = compile' e var env in
                 let load =
                   _assign var (_load (_binop (_var var) Plus (_int 4)))
                 in
                   _seq [s;(_exp load)]
             | _ -> error "PrimApp applied with wrong # of args")
      | Scish_ast.Lambda(v,e) ->
          let t = new_var "f" in
          let body = compile' e "result" (insert v env) in
          let _ = mk_fun t body ["dynenv"] ["result"] in
          let malloc = _assign var (_malloc 8) in
          let store1 = _store (_var var) (_var t) in
          let store2 = _store (_binop (_var var) Plus (_int 4)) (_var "dynenv") in
            _seq [(_exp malloc);(_exp store1);(_exp store2)]
      | Scish_ast.App(e1,e2) ->
          let func = compile' e1 var env in
          let t1 = new_var "t" in
          let t2 = new_var "t" in
          (* s1 contains function *)
          let s1 = _assign t1 (_load (_var var)) in
          (* s2 contains dynenv *)
          let s2 = _assign t2 (_load (_binop (_var var) Plus (_int 4))) in
          let arg_name = new_var "t" in
          let arg = compile' e2 arg_name env in
          let dynenv1 = _store (_var var) (_var arg_name) in
          let dynenv2 = _store (_binop (_var var) Plus (_int 4)) (_var t2) in
          let fun_call = _assign var (_call (_var t1) [(_var var)]) in
          let s = _seq [func;(_exp s1);
                        (_exp s2);arg;
                        (_exp dynenv1);
                        (_exp dynenv2);
                        (_exp fun_call)] 
          in
            (_let t1 (_let t2 (_let arg_name s)))
      | Scish_ast.If(test,then_branch, else_branch) ->
          let t_name = new_var "t" in
          let test_exp = compile' test t_name env in
          let then_stmt = compile' then_branch var env in
          let else_stmt = compile' else_branch var env in
          let s = _seq [test_exp;(If (_var t_name, then_stmt, else_stmt), 0)] in
            _let t_name s
  in

    let main_stmt = compile' e "result" [] in
    let _ = mk_fun "main" main_stmt [] ["result";"dynenv"] in
    List.map (function e -> Fn e) (List.rev !fun_env)
;;
