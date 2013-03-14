type var = string   (* program variables *)
type tvar = string  (* type variables *)
type pos = int

type tipe = 
  Tvar_t of tvar
| Int_t
| Bool_t
| Unit_t
| Fn_t of tipe * tipe
| Pair_t of tipe * tipe
| List_t of tipe
| Guess_t of tipe option ref
;;

type tipe_scheme = Forall of (tvar list) * tipe;;

type prim = 
  Int of int
| Bool of bool
| Unit   (* unit value -- () *)
| Plus   (* add two ints *)
| Minus  (* subtract two ints *)
| Times  (* multiply two ints *)
| Div    (* divide two ints *)
| Eq     (* compare two ints for equality *)
| Lt     (* compare two ints for inequality *)
| Pair   (* create a pair from two values *)
| Fst    (* fetch the 1st component of a pair *)
| Snd    (* fetch the 2nd component of a pair *)
| Nil    (* the empty list *)
| Cons   (* create a list from two values *)
| IsNil  (* determine whether a list is Nil *)
| Hd     (* fetch the head of a list *)
| Tl     (* fetch the tail of a list *)
;;

type rexp = 
  Var of var
| PrimApp of prim * exp list
| Fn of var * exp
| App of exp * exp
| If of exp * exp * exp
| Let of var * exp * exp
and exp = rexp * pos
;;

let rec tipe2string (t:tipe) : string =
  match t with
      Tvar_t(tvar) -> Printf.sprintf "Tvar_t(%s)" tvar
    | Int_t -> "Int_t"
    | Bool_t -> "Bool_t"
    | Unit_t -> "Unit_t"
    | Fn_t(t1,t2) -> Printf.sprintf "Fn_t(%s,%s)" (tipe2string t1) (tipe2string t2)
    | Pair_t(t1,t2)-> Printf.sprintf "Pair_t(%s,%s)" (tipe2string t1) (tipe2string t2)
    | List_t(t) -> Printf.sprintf "List_t(%s)" (tipe2string t)
    | Guess_t({contents = (Some t)}) -> Printf.sprintf "Guess_t(%s)" (tipe2string t)
    | Guess_t({contents = None}) -> "Guess_t(None)"
;;
