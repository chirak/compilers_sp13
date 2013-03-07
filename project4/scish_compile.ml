open Cish_ast

exception Unimplemented

(* generate fresh labels *)
let var_counter = ref 0;;
let new_int() = (var_counter := (!var_counter) + 1; !var_counter);;
let new_var() = "v_" ^ (string_of_int (new_int()));;

type value = int ;;
type env = 
    Empty
  | Frame of (var * value) * env
;;

exception Error
let error s = (print_string s; print_string "\n"; raise Error)

(* Lookup a variable in the environment -- return the 1st binding
 * corresponding to the variable. *)          
let rec lookup (x:var) (env:env) : value = 
    match env with
      Empty -> error ("Unbound variable "^x)
    | Frame ((y,v),rest) -> if (y = x) then v else lookup x rest
;;

let rec create_temps (env:env) (count:int) : env =
  if count <= 0 then
    env
  else
    create_temps (Frame((new_var(), 0), env)) (count-1)
;;

let compile_exp (e:Scish_ast.exp) : program = 
    let fun_env : funcsig list ref = ref [] in

    let compile_prim (expr:Scish_ast.exp) (env:env) : Cish_ast.exp =
      match expr with
          Scish_ast.Int(i) -> (Int(i), 0)
        | Scish_ast.Var(v) -> (Var(v), 0)
        | _ -> error "Got something other than int or var"
    in

    let compile2binop fst snd op env : stmt =
      (Exp(Binop(compile_prim fst env, op, compile_prim snd env), 0), 0)
    in


    let rec compile' (expr:Scish_ast.exp) (env:env) : Cish_ast.stmt =

      let new_cons t e1 e2 : stmt =
        let v1 = (Store((Var("result"), 0), (compile_prim e1 Empty)), 0) in
        let v2 = (Store((Binop((Var("result"), 0), Plus, (Int(4), 0)), 0),
                        (compile_prim e2 Empty)), 0)
        in
        let fn_body =
           (Let("result",
                (Malloc(Int(8), 0), 0),
                (Seq((Exp v1, 0),
                     (Seq((Exp v2, 0), (Return((Var("result"), 0)), 0)),
                      0)), 0)), 0)
        in
          fun_env := { name=t; args=[]; body=fn_body; pos=0 }::!fun_env;
          (Exp(Assign("result", (Var(t), 0)), 0), 0)
      in

      match expr with
          Scish_ast.Int(i) -> (Exp(Int(i), 0), 0)
        | Scish_ast.Var(v) -> (Exp(Var(v), 0), 0)
        | Scish_ast.PrimApp(op, exprs) ->
            (match (op,exprs) with
                 (Scish_ast.Plus,[e1;e2])  -> compile2binop e1 e2 Plus env
               | (Scish_ast.Minus,[e1;e2]) -> compile2binop e1 e2 Minus env
               | (Scish_ast.Times,[e1;e2]) -> compile2binop e1 e2 Times env
               | (Scish_ast.Div,[e1;e2])   -> compile2binop e1 e2 Div env
               | (Scish_ast.Eq,[e1;e2])    -> compile2binop e1 e2 Eq env
               | (Scish_ast.Lt,[e1;e2])    -> compile2binop e1 e2 Lt env
               | (Scish_ast.Cons,[hd;tl]) ->
                   let t = new_var() in 
                    (* Returns (Exp(Assign("result", t)) i.e. result = t *)
                     new_cons t hd tl 
               | (Scish_ast.Fst,[e]) -> (* e should be a cons *)
                   let v = compile' e env in
                     (Seq(v, (Exp(Assign( "result",
                                          (Load (Call((Var("result"), 0), []), 0), 0)
                     ), 0),0) ), 0)
               | (Scish_ast.Snd,[e]) ->
                   let v = compile' e env in
                     (Seq(v,
                          (Exp(Assign( "result",
                                   (Load (Binop((Call((Var("result"), 0), []), 0),
                                          Plus,
                                          (Int(4), 0)), 0), 0)
                          ), 0),0) ), 0)
               | _ -> error "Error")
        | Scish_ast.Lambda(var, exp) ->
            (* Generate new function that takes in one argument, the env.
               Produce a closure, pair of the function name and the
               environment. In this case we can make a cons which will
               require memory allocation on the heap *)
            raise Unimplemented
        | Scish_ast.App(e1, e2) ->
            (* Evaluate e1 and e2 to v1 and v2, where v1 is a closure
               Extract the function pointer and env of the clsure and
               add v2 to the env. Invoke the function *)
            raise Unimplemented
        | Scish_ast.If(test, then_branch, else_branch) -> raise Unimplemented
        | _ -> raise Unimplemented
    in
    let main_body =
        (Let("dynenv", (Int(0), 0),
             (Let("result",
                  (Int(0), 0),
                  (Seq((compile' e Empty),
                       (Return((Var("result"), 0)), 0)),
                  0)),
              0)
        ), 0)
    in
    let main = { name = "main"; args = []; body = main_body; pos = 0 } in
    List.map (function e -> Fn e) (List.rev (main :: !fun_env))

;;

(* COMPILING LAMBDA EXPRESSION *)
(* let new_fun_name = new_var() in *)
(* let body = compile' fun_env param::env in *)
(* let _ = fun_env := { name=new_fun_name; args=["env"]; body=body; pos=0 }::fun_env; *)
(* (Exp((Var(new_fun_name), 0)), 0) *)
