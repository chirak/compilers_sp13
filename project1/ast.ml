(* The abstract syntax for our little subset of Fortran *)
type var = string
type pos = int      (* position is line number in source file *)

type binop = 
  Plus | Minus | Times | Div          (* +, -, *, /           *)
| Eq | Neq | Lt | Lte | Gt | Gte      (* ==, !=, <, <=, >, >= *)

type rexp = 
  Int of int
| Var of var
| Binop of exp * binop * exp
| Not of exp                          (* !x *)
| And of exp * exp                    (* x < y && y < z *)
| Or of exp * exp                     (* x < y || x < z *)
| Assign of var * exp                 (* x = y+42 *)

(* every expression comes with its position *)
and exp = rexp * pos

type rstmt = 
  Exp of exp                          (* x = 3+4; *)
| Seq of stmt * stmt                  (* x = 2*9; y = 42; *)
| If of exp * stmt * stmt             (* if (x == y) x = 42 else y = 43 *)
| While of exp * stmt                 (* while (x < y) x = x + 1; *)
| For of exp * exp * exp * stmt       (* for (x=0; x<y; x=x+1) y=y*42; *)
| Return of exp                       (* return e; *)

(* every statement comes with its position *)
and stmt = rstmt * pos

let skip : rstmt = Exp(Int 0,0)          (* simulate a skip statement *)

type program = stmt

(*
 *
Sums     - Sums + Products
Sums     - Products
Products - Products * Value
Products - Value



Value    - int
Value    - id

rexpr:
  NOT binop { (Not($2), 0) }
| binop AND binop { (And($1, $3), 0) }
| binop OR binop { (Or($1, $3), 0) }
| VAR EQUAL rexpr { (Assign($1, $3), 0) }
| binop { $1 }
;

binop:
  binop PLUS prod { (Binop($1, Plus, $3), 0) }
| binop MINUS prod { (Binop($1, Minus, $3), 0) }
| binop EQ prod { (Binop($1, Eq, $3), 0) }
| binop NEQ prod { (Binop($1, Neq, $3), 0) }
| binop LT prod { (Binop($1, Lt, $3), 0) }
| binop LTE prod { (Binop($1, Lte, $3), 0) }
| binop GT prod { (Binop($1, Gt, $3), 0) }
| binop GTE prod { (Binop($1, Gte, $3), 0) }
| prod { $1 }
;

prod:
  prod TIMES value { (Binop($1, Times, $3), 0) }
| prod DIVIDE value { (Binop($1, Div, $3), 0) }
| value { $1 }
;
*)







