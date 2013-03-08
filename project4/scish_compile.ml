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
  
  let rec compile' (expr:Scish_ast.exp) (var:string) : Cish_ast.stmt =
      let _let_ (var:string) (s:stmt) =
        (Let (var, (Int(0), 0), s), 0)
      in

    let compile_prim op e1 e2 =
      let seq3 s1 s2 s3 =
        (Seq (s1, (Seq(s2, s3), 0)), 0)
      in
      let _binop_ v1 op v2 = 
        (Binop((Var v1, 0), op, (Var v2, 0)), 0)
      in

      let t1 = new_var() in
      let t2 = new_var() in
      let v1 = compile' e1 t1 in
      let v2 = compile' e2 t2 in
      let res = (Exp (Assign (var, (_binop_ t1 op t2)), 0),0) in
      let s = seq3 v1 v2 res in
        (_let_ t1 (_let_ t2 s))
    in

    let compile_cons fst snd =
      let seq s1 s2 s3 s4 s5 =
        (Seq (s1, (Seq(s2, (Seq (s3, (Seq (s4, s5), 0)), 0)), 0)), 0)
      in

      let t1 = new_var() in
      let t2 = new_var() in
      let v1 = compile' fst t1 in
      let v2 = compile' snd t2 in
      let malloc = (Assign (var, (Malloc (Int(8), 0), 0)), 0) in
      let store1 = (Store((Var(var), 0), (Var t1, 0)), 0) in
      let store2 = (Store((Binop((Var(var), 0), Plus, (Int(4), 0)), 0),(Var t2, 0)),0) in
      let s = (seq v1 v2 (Exp malloc, 0) (Exp store1, 0) (Exp store2, 0)) in
        (_let_ t1 (_let_ t2 s))
    in

    match expr with
        Scish_ast.Int(i) -> (Exp (Assign (var, (Int(i), 0)), 0), 0)
      | Scish_ast.Var(v) -> (Exp (Assign (var, (Var(v), 0)), 0), 0)
      | Scish_ast.PrimApp(op, exprs) ->
          (match (op, exprs) with
               (Scish_ast.Plus,[e1;e2])  -> compile_prim Plus e1 e2
             | (Scish_ast.Minus,[e1;e2]) -> compile_prim Minus e1 e2
             | (Scish_ast.Times,[e1;e2]) -> compile_prim Times e1 e2
             | (Scish_ast.Div,[e1;e2])   -> compile_prim Div e1 e2
             | (Scish_ast.Eq,[e1;e2])    -> compile_prim Eq e1 e2
             | (Scish_ast.Lt,[e1;e2])    -> compile_prim Lt e1 e2
             | (Scish_ast.Cons,[hd;tl])  -> compile_cons hd tl
             | (Scish_ast.Fst,[e]) ->
                 let s = compile' e var in (* this must be a cons, fst, or snd *)
                 let load = (Assign(var, (Load (Var(var), 0), 0)), 0) in
                   (Seq(s, (Exp load, 0)), 0)
             | (Scish_ast.Snd,[e]) ->
                 let s = compile' e var in (* this must be a cons, fst, or snd *)
                 let load = 
                   (Assign(var,
                           (Load (Binop((Var(var), 0),
                                        Plus,
                                        (Int (4), 0)), 0), 0)), 0)
                 in
                   (Seq(s, (Exp load, 0)), 0)
             | _ -> error "PrimApp applied with wrong # of args")
      | Scish_ast.Lambda(v,e) -> raise Unimplemented
      | Scish_ast.App(e1,e2) -> raise Unimplemented
      | Scish_ast.If(test,then_branch, else_branch) -> raise Unimplemented
  in

    let main_stmt = compile' e "result" in
    let _ = mk_fun "main" main_stmt [] ["result";"dynenv"] in
    List.map (function e -> Fn e) (List.rev !fun_env)
;;
