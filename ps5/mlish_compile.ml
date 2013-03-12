module ML = Mlish_ast;;
module S = Scish_ast;;

exception Unimplemented;;

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
    | ML.Nil -> raise Unimplemented
    | ML.Cons -> S.Prim(S.Cons)
    | ML.IsNil -> raise Unimplemented
    | ML.Hd -> S.Prim(S.Fst)
    | ML.Tl -> S.Prim(S.Snd)
;;

let rec compile_exp ((e,_):ML.exp) : S.exp =
  match e with
      ML.Var(v) -> S.Var(v)
    | ML.PrimApp(p, exps) ->
        let p' = compile_prim p in
        let exps'' = List.map compile_exp exps in
          (match p' with
               S.Prim(op) -> S.PrimApp(op, exps'')
             | S.Exp(e)   -> e)
    | ML.Fn(v, exp) ->
        S.Lambda(v, compile_exp exp)
    | ML.App(e1, e2) -> 
        S.App(compile_exp e1, compile_exp e2)
    | ML.If(test, e1, e2) -> 
        S.If(compile_exp test, compile_exp e1, compile_exp e2)
    | ML.Let(v, e1, e2) ->
        let l = S.Lambda(v, compile_exp e1) in
          S.App(l, compile_exp e2)
;;
