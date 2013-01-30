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
%token FOR WHILE
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

stmt :
  RETURN binop SEMI { (Return $2, 1) }
;

binop:
  LPAREN binop RPAREN { }
| binop PLUS binop { (Binop($1, Plus, $3), 0) }
| binop MINUS binop { (Binop($1, Minus, $3), 0) }
| binop EQ binop { (Binop($1, Eq, $3), 0) }
| prod { $1 }
;

prod:
  LPAREN prod RPAREN { $2 }
| prod TIMES prod { (Binop($1, Times, $3), 0) }
| prod DIVIDE prod { (Binop($1, Div, $3), 0) }
| value { $1 }
;

value:
  INT { (Int($1), 1) }
| VAR { (Var($1), 1) }
;



