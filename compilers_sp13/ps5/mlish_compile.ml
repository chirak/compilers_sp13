module ML = Mlish_ast;;
module S = Scish_ast;;

exception Unimplemented;;
exception Error;;
exception EnvLookup
let error s = (print_string s; print_string "\n"; raise Error);;

let var_counter = ref 0;;
let new_int() = (var_counter := (!var_counter) + 1; !var_counter);;
let new_var() = "t" ^ (string_of_int (new_int()));;

type env = (string) list;;

let rec lookup (v:string) (env:env) : bool =
  match env with
      [] -> raise EnvLookup
    | hd::tl ->
        v = hd || lookup v tl
;;

let compile_prim (prim:ML.prim) : S.union =
  match prim with
      ML.Int(i)  -> S.Exp(S.Int(i))
    | ML.Bool(b) ->
        (match b with
             true -> S.Exp(S.Int(1))
           | false -> S.Exp(S.Int(0)))
    | ML.Unit -> S.Exp(S.Int(0))
    | ML.Plus -> S.Prim(S.Plus)
    | ML.Minus -> S.Prim(S.Minus)
    | ML.Times -> S.Prim(S.Times)
    | ML.Div -> S.Prim(S.Div)
    | ML.Eq -> S.Prim(S.Eq)
    | ML.Lt -> S.Prim(S.Lt)
    | ML.Pair -> S.Prim(S.Cons)
    | ML.Fst -> S.Prim(S.Fst)
    | ML.Snd -> S.Prim(S.Snd)
    | ML.Nil -> S.Exp(S.Int(0))
    | ML.Cons -> S.Prim(S.Cons)
    | ML.IsNil -> S.Exp(S.PrimApp(S.Eq, [S.Int(0);S.Var("x")]))
    | ML.Hd -> S.Prim(S.Fst)
    | ML.Tl -> S.Prim(S.Snd)
;;

let rec compile_exp (expr:ML.exp) : S.exp =
  let rec compile' ((e,_):ML.exp) (env:env) : S.exp =
    match e with
        ML.Var(v) -> 
          if lookup v env then S.Var(v) else raise EnvLookup
      | ML.PrimApp(p, exps) ->
          let p' = compile_prim p in
          let exps' = List.map (fun ex -> compile' ex env) exps in
            (match p' with
                 S.Prim(op) -> S.PrimApp(op, exps')
               | S.Exp(e)   -> e)
      | ML.Fn(v, exp) ->
          S.Lambda(v, compile' exp (v::env))
      | ML.App(e1, e2) -> 
          S.App(compile' e1 env, compile' e2 env)
      | ML.If(test, e1, e2) -> 
          S.If(compile' test env, compile' e1 env, compile' e2 env)
      | ML.Let(v, e1, e2) ->
          compile' (ML.App((ML.Fn(v, e2), 0), e1), 0) env
  in
    compile' expr []
;;
