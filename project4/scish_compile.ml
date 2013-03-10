open Cish_ast

exception Unimplemented
exception Error
let error s = (print_string s; print_string "\n"; raise Error);;

(* generate fresh variables *)
let var_counter = ref 0;;
let new_int() = (var_counter := (!var_counter) + 1; !var_counter);;
let new_var() = "v_" ^ (string_of_int (new_int()));;

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
  
  let rec compile' (expr:Scish_ast.exp) (var:string) (env:(string*int) list) : Cish_ast.stmt =
      let _let_ (var:string) (s:stmt) =
        (Let (var, (Int(0), 0), s), 0)
      in

    let seq3 s1 s2 s3 =
      (Seq (s1, (Seq(s2, s3), 0)), 0)
    in

    let seq5 s1 s2 s3 s4 s5 =
      (Seq (s1, (Seq(s2, (Seq (s3, (Seq (s4, s5), 0)), 0)), 0)), 0)
    in

    let seq6 s1 s2 s3 s4 s5 s6 =
      (Seq (s1, (Seq(s2, (Seq (s3, (Seq (s4, (Seq (s5, s6), 0)), 0)), 0)), 0)), 0)
    in

    let compile_prim op e1 e2 =
      let _binop_ v1 op v2 = 
        (Binop((Var v1, 0), op, (Var v2, 0)), 0)
      in

      let t1 = new_var() in
      let t2 = new_var() in
      let v1 = compile' e1 t1 env in
      let v2 = compile' e2 t2 env in
      let res = (Exp (Assign (var, (_binop_ t1 op t2)), 0),0) in
      let s = seq3 v1 v2 res in
        (_let_ t1 (_let_ t2 s))
    in

    let compile_cons fst snd v =

      let t1 = new_var() in
      let t2 = new_var() in
      let v1 = compile' fst t1 env in
      let v2 = compile' snd t2 env in
      let malloc = (Assign (v, (Malloc (Int(8), 0), 0)), 0) in
      let store1 = (Store((Var(v), 0), (Var t1, 0)), 0) in
      let store2 = (Store((Binop((Var(v), 0), Plus, (Int(4), 0)), 0),(Var t2, 0)),0) in
      let s = (seq5 v1 v2 (Exp malloc, 0) (Exp store1, 0) (Exp store2, 0)) in
        (_let_ t1 (_let_ t2 s))
    in

    let insert v env =
      let offset = (List.length env) + 1 in
        (v,offset)::env
    in

    let rec in_env (v:string) (env:(string * int) list) : bool =
      match env with
          [] -> false
        | hd::tl -> 
            let v', _ = hd in
              v = v' || in_env v tl
    in

    let rec s_lookup (v:string) (env: (string * int) list) =
      match env with
          [] -> error (Printf.sprintf "Variable %s not found in env" v)
        | hd::tl ->
            let v', off = hd in
              if v = v' then off else s_lookup v tl
    in

    let rec c_lookup (i:int) (accum:exp) : exp =
      if i <= 0 then
        (Load accum, 0)
      else
        c_lookup (i-1) (Load (Binop(accum, Plus, (Int(4),0)), 0), 0)
    in

    match expr with
        Scish_ast.Int(i) -> (Exp (Assign (var, (Int(i), 0)), 0), 0)
      | Scish_ast.Var(v) ->
          if in_env v env then
            let off = (List.length env) - (s_lookup v env) in
            let load = c_lookup off (Var "dynenv", 0) in
              (Exp (Assign (var, load), 0), 0)
          else
            (Exp (Assign (var, (Var(v), 0)), 0), 0)
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
                 let load = (Assign(var, (Load (Var(var), 0), 0)), 0) in
                   (Seq(s, (Exp load, 0)), 0)
             | (Scish_ast.Snd,[e]) ->
                 let s = compile' e var env in
                 let load = 
                   (Assign(var,
                           (Load (Binop((Var(var), 0),
                                        Plus,
                                        (Int (4), 0)), 0), 0)), 0)
                 in
                   (Seq(s, (Exp load, 0)), 0)
             | _ -> error "PrimApp applied with wrong # of args")
      | Scish_ast.Lambda(v,e) ->
          let t = new_var() in
          let body = compile' e "result" (insert v env) in
          let _ = mk_fun t body ["dynenv"] ["result"] in
            compile_cons (Scish_ast.Var t) (Scish_ast.Var "dynenv") var
      | Scish_ast.App(e1,e2) ->
          let func = compile' e1 var env in
          let t1 = new_var() in
          let t2 = new_var() in
          (* s1 contains function *)
          let s1 = (Assign(t1, (Load (Var(var), 0), 0)), 0) in
          (* s2 contains dynenv *)
          let s2 = (Assign(t2,
                           (Load (Binop((Var(var), 0),
                                        Plus,
                                        (Int (4), 0)), 0), 0)), 0)
          in

          let arg_name = new_var() in
          let arg = compile' e2 arg_name env in
          let arg_env = compile_cons (Scish_ast.Var arg_name) (Scish_ast.Var t2) "result" in
          let fun_call = (Exp (Assign (var, (Call ((Var t1, 0), [(Var var, 0)]), 0)), 0), 0) in
          (*s1, s2, arg, arg_env, fun_call *)
          let seq = seq6 func (Exp s1, 0) (Exp s2, 0) arg arg_env fun_call in
            (_let_ t1 (_let_ t2 (_let_ arg_name seq)))
      | Scish_ast.If(test,then_branch, else_branch) -> raise Unimplemented
  in

    let main_stmt = compile' e "result" [] in
    let _ = mk_fun "main" main_stmt [] ["result";"dynenv"] in
    List.map (function e -> Fn e) (List.rev !fun_env)
;;
