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

%left EQUAL
%left AND OR NOT
%left EQ NEQ GT GTE LT LTE
%left PLUS MINUS
%left DIVIDE TIMES

/* Here's where the real grammar starts -- you'll need to add 
 * more rules here... Do not remove the 2%'s!! */
%%

program:
  stmt EOF { $1 }
;

stmt:
  LBRACE stmt RBRACE { $2 }
| rexp SEMI { (Exp $1, rhs 0) }
| stmt stmt { (Seq($1, $2), rhs 0) }
| IF rexp stmt 
    { (If($2, $3, (Ast.skip, rhs 0)), rhs 0) }
| IF rexp stmt ELSE stmt 
    { (If($2, $3, $5), rhs 0) }
| WHILE rexp LBRACE stmt RBRACE
    { (While($2, $4), rhs 0) }
| FOR LPAREN rexp SEMI rexp SEMI rexp RPAREN LBRACE stmt RBRACE
    { (For($3, $5, $7, $10), rhs 0) }
| RETURN rexp SEMI { (Return $2, rhs 0) }
;

rexp:
  VAR EQUAL rexp { (Assign($1, $3), rhs 0) }
| LPAREN rexp RPAREN { $2 }
| rexp AND rexp { (And($1, $3), rhs 0) }
| rexp OR rexp { (Or($1, $3), rhs 0) }
| NOT rexp { (Not ($2), rhs 0) }
| rexp EQ rexp { (Binop($1, Eq, $3), rhs 0) }
| rexp NEQ rexp { (Binop($1, Neq, $3), rhs 0) }
| rexp GT rexp { (Binop($1, Gt, $3), rhs 0) }
| rexp GTE rexp { (Binop($1, Gte, $3), rhs 0) }
| rexp LT rexp { (Binop($1, Lt, $3), rhs 0) }
| rexp LTE rexp { (Binop($1, Lte, $3), rhs 0) }
| rexp PLUS rexp { (Binop($1, Plus, $3), rhs 0) }
| rexp MINUS rexp { (Binop($1, Minus, $3), rhs 0) }
| rexp TIMES rexp { (Binop($1, Times, $3), rhs 0) }
| rexp DIVIDE rexp { (Binop($1, Div, $3), rhs 0) }
| INT { (Int($1), 1) }
| VAR { (Var($1), 1) }
;

