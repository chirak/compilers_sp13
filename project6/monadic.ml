(* Defines our Monadic Intermediate Form, conversion of Scish AST to
 * Monadic Form, and various optimizations on Monadic Form. *)
module S = Scish_ast
type var = string

exception Error
exception TODO
exception EXTRA_CREDIT

(* operands -- pure and small *)
type operand = Var of var | Int of int;;

(* values -- pure, but possibly large *)
type value = 
  Op of operand
| PrimApp of S.primop * (operand list)
| Lambda of var * exp
(* expressions -- possibly impure, control flow *)
and exp = 
  Return of operand
| LetVal of var * value * exp
| LetCall of var * operand * operand * exp
| LetIf of var * operand * exp * exp * exp
;;

(* convert a monadic expression to a string *)
let exp2string (e:exp) : string = 
    let o2s = function
      (Var x) -> x
    | (Int i) -> string_of_int i in
    let rec e2s (tab:string) (e:exp) = 
        match e with
          Return w -> tab ^ (o2s w) ^ "\n"
        | LetCall(x,w1,w2,e) -> 
            tab ^"let "^x^" = "^(o2s w1)^"("^(o2s w2)^")\n"^(e2s tab e)
        | LetIf(x,w,e1,e2,e) -> 
            tab^"let "^x^" = if "^(o2s w)^" then\n"^
            (e2s (tab^"           ") e1)^
                  tab^"         else\n"^
            (e2s (tab^"           ") e2)^
            (e2s tab e)
        | LetVal(x,Op w,e) -> "let "^x^" = "^(o2s w)^"\n"^(e2s tab e)
        | LetVal(x,PrimApp(p,ws),e) -> 
            tab^"let "^x^" = "^(S.primop2string p)^"("^
            (String.concat "," (List.map o2s ws))^")\n"^(e2s tab e)
        | LetVal(x,Lambda(y,e1),e2) -> 
            tab^"fun "^x^"("^y^") =\n"^(e2s (tab^"     ") e1)^(e2s tab e2) in
    e2s "" e
;;

(* used to generate fresh variables *)
let counter = ref 0;;
let fresh_var() = 
    let c = !counter in
    counter := c+1; "x"^(string_of_int c)
;;

(* naive var->var environments *)
exception UnboundVariable of string;;
let empty_env (x:var):var = (print_string ("unbound variable "^x); 
                             raise (UnboundVariable x));;
let extend (env:var->var) (x:var) (y:var) =
    fun z -> if z = x then y else env z;;

(* convert an expression e to monadic form:
 *   env is used to rename variables on the fly so they are unique.
 *   k is used as a continuation as explained below:
 * 
 * Conceptually, each Scish expression compiles down to a
 * monadic expression of the form Let(x1,v1,...,Let(xn,vn),Return v).
 * That is, it's a sequence of let-bindings followed by a return
 * of an operand.
 *
 * Consider what happens when we have a compound expression
 * such as e1+e2.  If we had nested lets, then we could write:
 *    Let(x1,compile e1,
 *    Let(x2,compile e2,
 *    LetVal(x3,PrimApp(Plus,[Var x1,Var x2]),
 *    Return (Var x3))))
 * but we don't have nested lets.  In general, we'll have to
 * take the let-expressions and operand for e1 and flatten it
 * out using the equation:
 *
 *  (let x = (let y = a in b) in c) == let y = a in let x = b in c
 *
 * Similarly, we'll have to flatten out the translation of e2.
 * But all of this flattening amounts to appending lists of 
 * let-declarations.  If we're not careful, we end up with a 
 * quadractic algorithm (the same problem we had with lowering to MIPS.)
 *
 * To solve this problem, we use a clever trick which allows us
 * to flatten on-the-fly.  The trick is to parameterize the 
 * translation with a function, which when given an operand,
 * generates "the rest of the translation".  This extra parameter,
 * called a continuation, is essentially a functional accumulator.
 *
 * Consider the case for tom App(e1,e2) Return
 * Imagine that the monadic form of the expressions is
 *    e1 == Let x1=a1 in...Let xn=an in Return w1
 *    e2 == Let y1=b1 in...Let ym=bm in Return w2
 * Then we should get as output something like:
 *   Let x1=a1...in Let xn=an in 
 *    Let y1=b1 ...in Let ym=bm in Let z=w1(w2) in Return z
 *
 * Following the definitions, we have:
 *   tom (App(e1,e2)) Return = tom e1 k1
 *     where k1 = (fn w1-> tom e2 (fn w2 -> Let z=w1(w2) in Return z))
 *   tom e1 k1 = 
 *     Let x1=a1 in ... Let xn=an in k1(w1)    (by assumption)
 *
 *   k1(w1) = tom e2 (fn w2 -> Let z=w1(w2) in Return z)
 *          = Let y1=b1 in ... Let ym=bm in Let z=w1(w2) in Return z
 * So tom e1 k1 = 
 *     Let x1=a1 in ... Let xn=an in 
 *       Let y1=b1 in ...Let ym=bm in Let z=w1(w2) in Return z
 *)
let rec tom (e : S.exp) (env:var->var) (k : operand -> exp) : exp = 
    match e with
      S.Var x -> k (Var (env x))
    | S.Int i -> k (Int i)
    | S.App(S.Lambda(x,e1),e2) -> (* same as let x = e2 in e1 *)
        let x' = fresh_var() in
        tom e2 env (fun w1 -> LetVal(x',Op w1,tom e1 (extend env x x') k))
    | S.App(e1,e2) -> 
        let x = fresh_var() in
        tom e1 env (fun w1 -> tom e2 env (fun w2 -> LetCall(x,w1,w2,k(Var x))))
    | S.Lambda(x,e) -> 
        let x' = fresh_var() in
        let f = fresh_var() in
        LetVal(f,Lambda(x',tom e (extend env x x') (fun x -> Return x)),k(Var f))
    | S.PrimApp(p,es) -> 
        let x = fresh_var() in
        toms es [] env (fun ws -> LetVal(x,PrimApp(p,ws),k(Var x)))
    | S.If(e1,e2,e3) -> 
        let x = fresh_var() in
        tom e1 env 
              (fun w -> LetIf(x,w,tom e2 env (fun x -> Return x),
                              tom e3 env (fun x -> Return x),k(Var x)))
and toms (es : S.exp list) (accum: operand list) 
           (env : var->var) (k: operand list -> exp) = 
    match es with
      [] -> k(List.rev accum)
    | e::rest -> tom e env (fun w -> toms rest (w::accum) env k)
;;

let tomonadic (e:S.exp) : exp = tom e empty_env (fun x -> Return x);;

(* a flag used to track when an optimization makes a reduction *)
let changed : bool ref = ref true;;
let change x = (changed := true; x);;

(* naive 'a -> 'b option environments *)
let empty_env x = None;;
let extend env x w = fun y -> if y = x then Some w else env y;;

(* operand propagation -- LetVal(x,Op w,e) --> e[w/x] -- just like notes. *)
let rec cprop_exp (env : var -> operand option) (e:exp) = 
    match e with
      Return w -> Return (cprop_oper env w)
    | LetVal(x,Op w,e) -> 
        change(cprop_exp (extend env x (cprop_oper env w)) e)
    | LetVal(x,PrimApp(p,ws),e) -> 
        LetVal(x,PrimApp(p,List.map (cprop_oper env) ws),cprop_exp env e)
    | LetVal(x,Lambda(y,e1),e2) -> 
        LetVal(x,Lambda(y,cprop_exp env e1),cprop_exp env e2)
    | LetCall(x,w1,w2,e) -> 
        LetCall(x,cprop_oper env w1, cprop_oper env w2,cprop_exp env e)
    | LetIf(x,w,e1,e2,e) -> 
        LetIf(x,cprop_oper env w,cprop_exp env e1,cprop_exp env e2,
              cprop_exp env e)
and cprop_oper (env : var -> operand option) (w:operand) = 
    match w with
      Var x -> (match env x with None -> w | Some w' -> w')
    | Int _ -> w
;;

let cprop e = cprop_exp empty_env e;;

(* common sub-value elimination -- as in the slides *)
(* need to implement environment from value -> var option *)
let rec cse_exp(env:value->var option)(e:exp):exp =
  match e with
    | Return w -> Return w
    | LetVal(x,v,e) ->
        (match env v with
           | None -> LetVal(x,cse_val env v,
                              cse_exp (extend env v x) e)
           | Some y -> LetVal(x,Op(Var y),cse_exp env e))
    | LetCall(x,f,w,e) -> LetCall(x,f,w,cse_exp env e)
    | LetIf(x,w,e1,e2,e) -> 
        LetIf(x,w,cse_exp env e1,cse_exp env e2,
              cse_exp env e)
and cse_val env v = 
  match v with
    | Lambda(x,e) -> Lambda(x,cse_exp env e)
    | v -> v
;;

let cse (e : exp) : exp = cse_exp empty_env e;;

let rec flatten (x:var) (e1:exp) (e2:exp) : exp =
  match e1 with
    | Return w -> LetVal(x,Op w,e2)
    | LetVal(y,v,e1) ->
        LetVal(y,v,flatten x e1 e2)
    | LetCall(y,f,ws,e1) ->
        LetCall(y,f,ws,flatten x e1 e2)
    | LetIf(y,w,et,ef,ec) ->
        LetIf(y,w,et,ef,flatten x ec e2)
;;

(* constant folding
 * Apply primitive operations which can be evaluated. e.g. fst (1,2) = 1
 *)
let rec cfold (e : exp) : exp =
  match e with
    | Return w -> Return w
    | LetVal(x,v,e) ->
        LetVal(x, cfold_val v, cfold e)
    | LetCall(x,f,ws,e) ->
        LetCall(x,f,ws,cfold e)
    | LetIf(x,Int 1,e1,e2,e) ->
        cfold (flatten x e1 e)
    | LetIf(x,Int 0,e1,e2,e) ->
        cfold (flatten x e1 e)
    | LetIf(x,w,e1,e2,e) ->
        LetIf(x,w,cfold e1, cfold e2, cfold e)

and cfold_val (v:value) : value =
  match v with
    | Lambda(x,e) -> Lambda(x, cfold e)
    | PrimApp (S.Plus, [Int i;Int j]) -> Op(Int(i+j))
    | PrimApp (S.Plus,[Int 0;e]) -> Op(e)
    | PrimApp (S.Plus,[e;Int 0]) -> Op(e)
    | PrimApp (S.Minus,[Int i;Int j]) -> Op(Int(i-j))
    | PrimApp (S.Minus,[e;Int 0]) -> Op(e)
    | PrimApp (S.Times, [Int i;Int j]) -> Op(Int(i*j))
    | PrimApp (S.Times, [e;Int 0]) -> Op(Int(0))
    | PrimApp (S.Times, [Int 0;e]) -> Op(Int(0))
    | PrimApp (S.Times, [e;Int 1]) -> Op(e)
    | PrimApp (S.Times, [Int 1;e]) -> Op(e)
    | PrimApp (S.Times, [e;Int 2]) | PrimApp (S.Times, [Int 2;e]) ->
        PrimApp(S.Plus, [e;e])
    | PrimApp (S.Div, [Int 0;Int j]) -> Op(Int(0))
    | PrimApp (S.Div, [e; Int 1]) -> Op(e)
    | PrimApp (S.Lt, [Int i; Int j]) -> Op(Int(bool2int (i<j)))
    | PrimApp (S.Lt, [Var v1; Var v2]) when v1 = v2 -> Op(Int(0))
    | PrimApp (S.Eq, [Int i; Int j]) -> Op(Int(bool2int(i=j)))
    | PrimApp (S.Eq, [Var v1; Var v2]) when v1 = v2 -> Op(Int(1))
    | PrimApp (S.Fst, [e1;e2]) -> Op(e1)
    | PrimApp (S.Snd, [e1;e2]) -> Op(e2)
    | v -> v

and bool2int (b:bool) : int =
  if b then 1 else 0
;;


(* To support a somewhat more efficient form of dead-code elimination and
 * inlining, we first construct a table saying how many times each variable 
 * is used, and how many times each function is called.
 * This table is then used to reduce LetVal(x,v,e) to e when x is used
 * zero times, and to reduce LetVal(x,Lambda(y,e),...,LetCall(z,x,w,e2)...)
 * to (...LetVal(y,Op w,(Let(z,e,e2)))...) when x is used once and 
 * that use is a call site.
 *)
(* type cnt_table = (var,{uses:int ref,calls:int ref}) Hashtbl.hash_table *)
type entry = { uses : int ref; calls: int ref }
exception NotFound;;
let new_cnt_table() = 
    Hashtbl.create 101;;
let def (t) (x:var) = 
    Hashtbl.add t x {uses=ref 0;calls=ref 0};;
let inc r = (r := !r + 1);;
let use (t) (x:var) = 
    inc ((Hashtbl.find t x).uses);;
let call (t) (x:var) = 
    inc ((Hashtbl.find t x).calls);;
let get_uses (t) (x:var) : int = !((Hashtbl.find t x).uses);;
let get_calls (t) (x:var) : int = !((Hashtbl.find t x).calls);;

let count_table (e:exp) = 
    let table = new_cnt_table() in
    let def = def table in
    let use = use table in
    let call = call table in
    let rec occ_e e = 
      match e with
        Return w -> occ_o w
      | LetVal(x,v,e) -> (def x; occ_v v; occ_e e)
      | LetCall(x,Var f,w2,e) -> 
         (def x; use f; call f; occ_o w2; occ_e e)
      | LetCall(x,w1,w2,e) -> 
         (def x; occ_o w1; occ_o w2; occ_e e)
      | LetIf(x,w,e1,e2,e) -> (def x; occ_o w; occ_e e1; occ_e e2;
                               occ_e e)
    and occ_v v = 
      match v with
        Op w -> occ_o w
      | PrimApp(_,ws) ->  List.iter occ_o ws
      | Lambda(x,e) -> (def x; occ_e e)
        
    and occ_o oper = 
      match oper with
        Var x -> use x
      | Int _ -> () in
    occ_e e; table
;;

(* dead code elimination *)
let rec dce (e:exp) : exp =
  match e with
    | Return w -> Return w
    | LetVal(x,v,e) ->
        if occurs_exp x e then LetVal(x,v,dce e) else dce e
    |LetCall(x,f,w,e) -> LetCall(x,f,w, dce e)
    | LetIf (x,w,e1,e2,e) -> LetIf(x,w,dce e1, dce e2, dce e)

and occurs_operand x o : bool =
  match o with
    | Var w when x = w -> true
    | _ -> false

and occurs_value x v : bool =
  match v with
      Op w -> occurs_operand x w
    | PrimApp (_, l) -> ormap l (fun a -> occurs_operand x a)
    | Lambda (_,e) -> occurs_exp x e

and occurs_exp x e : bool =
  match e with
    | Return w -> occurs_operand x w
    | LetVal(_,_,e) -> occurs_exp x e
    | LetCall(x,f,w,e) -> 
        (occurs_operand x f) ||
        (occurs_operand x w) ||
        (occurs_exp x e)
    | LetIf(x,w,e1,e2,e) ->
        (occurs_operand x w) ||
        (occurs_exp x e1) ||
        (occurs_exp x e2) ||
        (occurs_exp x e)

and ormap l f =
  match l with
      [] -> false
    | hd::tl -> f hd || ormap tl f
;;

(* (1) inline functions 
 * (2) reduce LetIf expressions when the value being tested is a constant.
 * 
 * In both cases, we are forced to re-flatten out what would otherwise
 * be nested let-expressions.  Therefore, we use the "splice" helper
 * function to splice the two expressions together.   In particular,
 * splice x e1 e2 is equivalent to flattening out let x=e1 in e2.
 * Note, however, that in the case of inlining where the threshold is
 * above 1, we can end up duplicating the body of a function.  We must
 * restore the invariant that no bound variable is duplicated by renaming
 * each bound variable in the copy (else other optimizations will break
 * due to variable capture.)  
 *)
let splice x e1 e2 = 
    let rec splice_exp final (env : var -> operand option) (e:exp) = 
      match e with
        Return w -> 
          if final then LetVal(x,Op (cprop_oper env w),e2)
          else Return(cprop_oper env w)
      | LetVal(y,v,e) -> 
          let y' = fresh_var() in
          LetVal(y',loop_value env v,
                splice_exp final (extend env y (Var y')) e)
      | LetCall(y,w1,w2,e) -> 
          let y' = fresh_var() in
          LetCall(y',cprop_oper env w1,cprop_oper env w2,
                  splice_exp final (extend env y (Var y')) e)
      | LetIf(y,w,e1,e2,e) -> 
          let y' = fresh_var() in
          LetIf(y',cprop_oper env w, splice_exp false env e1,
                splice_exp false env e2, 
                splice_exp final (extend env y (Var y')) e)
    and loop_value env v = 
      match v with
        Op w -> Op (cprop_oper env w)
      | Lambda(y,e) -> 
          let y' = fresh_var() in 
          Lambda(y',splice_exp false (extend env y (Var y')) e)
      | PrimApp(p,ws) -> PrimApp(p,List.map (cprop_oper env) ws) in
  splice_exp true empty_env e1
;;

let always_inline_thresh (e : exp) : bool = true;;  (** Always inline **)
let never_inline_thresh  (e : exp) : bool = false;;(** Never inline  **)

(* return true if the expression e is smaller than i, i.e. it has fewer
 * constructors
 *)
let size_inline_thresh (i : int) (e : exp) : bool =
  let rec exp_size (accum:int) (e:exp) =
    match e with
        Return _ -> accum
      | LetVal(_,_,e) -> exp_size (accum + 1) e
      | LetCall(_,_,_,e) ->  exp_size (accum + 1) e
      | LetIf(_,_,e1,e2,e) ->
          let max_size = max (exp_size accum e1) (exp_size accum e2) in
            (exp_size (accum + max_size + 1) e)
  in

  let size = exp_size 0 e in
    size < i
;;

(* inlining 
 * only inline the expression e if (inline_threshold e) return true.
 *)
let inline (thresh: exp -> bool) (e:exp) : exp =
  let rec inline thresh (env:var->value option) e =
    match e with
        Return _ as r -> r
      | LetVal(y,v,e1) ->
          (match v with
             | Lambda(arg, body) ->
                 let i_body = inline thresh env body in
                 let v' = Lambda(arg, i_body) in
                 let env' = if thresh i_body then extend env y v' else env in
                   LetVal(y,v', inline thresh env' e1)
             | _ -> LetVal(y,v, inline thresh env e1))
      | LetCall(y, ((Var name) as f), ws, e1) ->
          (match env name with
               None -> LetCall(y, f, ws, inline thresh env e1)
             | Some(Lambda(arg, body)) ->
                 let inner = LetVal(arg, Op ws, body) in
                   flatten y inner e1
             | _ -> raise Error)
      | LetIf(y,w,et,ef,ec) ->
          LetIf(y,w, inline thresh env et, inline thresh env ef, inline thresh env ec)
      | _ -> raise Error
  in
    inline thresh empty_env e
;;

(* reduction of conditions
 * - Optimize conditionals based on contextual information, e.g.
 *   if (x < 1) then if (x < 2) then X else Y else Z =-> 
 *     if (x < 1) then X else Z
 *   (since x < 1 implies x < 2)
 * - This is similar to constant folding + logic programming
 *)
let redtest (e:exp) : exp = e (* raise EXTRA_CREDIT *)
 

(* optimize the code by repeatedly performing optimization passes until
 * there is no change. *)
let optimize inline_threshold e = 
    let opt = fun x -> cprop (redtest (cse (cfold ((inline inline_threshold) x)))) in
    let rec loop (i:int) (e:exp) : exp = 
      (if (!changed) then 
        let _ = changed := false in
        let e' = opt e in
        let _ = print_string ("\nAfter "^(string_of_int i)^" rounds:\n") in
        let _ = print_string (exp2string e') in
        loop (i+1) e'
      else e) in
    changed := true;
    loop 1 e
;;

