/* Parser for Fish --- TODO */

%{
open Ast
open Lexing
(* use this to get the line number for the n'th token *)
let rhs n =
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum
let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n") 
%}

/* Tells us which non-terminal to start the grammar with. */
%start program

/* This specifies the non-terminals of the grammar and specifies the
 * types of the values they build. Don't forget to add any new non-
 * terminals here.
 */
%type <Ast.program> program
%type <Ast.stmt> stmt

/* The %token directive gives a definition of all of the terminals
 * (i.e., tokens) in the grammar. This will be used to generate the
 * tokens definition used by the lexer. So this is effectively the
 * interface between the lexer and the parser --- the lexer must
 * build values using this datatype constructor to pass to the parser.
 * You will need to augment this with your own tokens...
 */
%token <int> INT 
%token <string> VAR
%token EQUAL SEMI RETURN
%token FOR WHILE IF ELSE
%token LPAREN RPAREN LBRACE RBRACE /* grouping tokens */
%token PLUS MINUS TIMES DIVIDE /* arithmatic op. tokens */
%token GT GTE LT LTE EQ NEQ AND OR NOT /* boolean op. tokens */
%token EOF

/* Here's where the real grammar starts -- you'll need to add 
 * more rules here... Do not remove the 2%'s!! */
%%

program:
  stmt EOF { $1 }
;

stmt:
  LBRACE stmt RBRACE { $2 }
| rexp SEMI { (Exp $1, 0) }
| stmt stmt { (Seq($1, $2), 0) }
| RETURN rexp SEMI { (Return $2, 1) }
;

rexp:
  LPAREN rexp RPAREN { $2 }
| VAR EQUAL rexp { (Assign($1, $3), 0) }
| rexp PLUS prod { (Binop($1, Plus, $3), 0) }
| rexp MINUS prod { (Binop($1, Minus, $3), 0) }
| rexp EQ prod { (Binop($1, Eq, $3), 0) }
| prod { $1 }
;

prod:
  LPAREN prod RPAREN { $2 }
| prod TIMES log { (Binop($1, Times, $3), 0) }
| prod DIVIDE log { (Binop($1, Div, $3), 0) }
| log { $1 }
;

log:
  LPAREN log RPAREN { $2 }
| log AND log { (And($1, $3), 0) }
| log OR log { (Or($1, $3), 0) }
| NOT log { (Not ($2), 0) }
| alog { $1 }
;

alog:
  LPAREN alog RPAREN { $2 }
| alog EQ alog { (Binop($1, Eq, $3), 0) }
| alog NEQ alog { (Binop($1, Neq, $3), 0) }
| alog GT alog { (Binop($1, Gt, $3), 0) }
| alog GTE alog { (Binop($1, Gte, $3), 0) }
| alog LT alog { (Binop($1, Lt, $3), 0) }
| alog LTE alog { (Binop($1, Lte, $3), 0) }
| value { $1 }
;

value:
  INT { (Int($1), 1) }
| VAR { (Var($1), 1) }
;

