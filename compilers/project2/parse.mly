/* Parser for Fish */

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

%start program

/* nonterminals */
%type <Ast.program> program
%type <Ast.stmt> stmts
%type <Ast.stmt> stmt
%type <Ast.exp> expr

/* terminals */
%token <int> INT 
%token <string> VAR
%token SEMI LPAREN RPAREN LBRACE RBRACE
%token PLUS MINUS TIMES DIVIDE
%token EQUAL NEQUAL GREATEREQ GREATER LESSEQ LESS
%token NOT AND OR
%token GETS
%token RETURN IF ELSE WHILE FOR
%token EOF

/* Precedence declarations for expressions */
%right GETS
%left OR
%left AND
%left EQUAL NEQUAL GREATEREQ GREATER LESS LESSEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left NOT UMINUS

/* Precedence declarations for handling dangling else */
%nonassoc NO_ELSE
%nonassoc ELSE

/* Grammar */
%%

program:
  stmts EOF { $1 }

stmts :
   | /* empty */ { (Ast.skip, 0) }
   | stmt stmts { (Seq ($1, $2), rhs 1) }

stmt :
   | SEMI { (Ast.skip, rhs 1) }
   | RETURN expr SEMI { (Return $2, rhs 1) }
   | expr SEMI { (Exp $1, rhs 1) }
   | LBRACE stmts RBRACE { $2 }
   | IF LPAREN expr RPAREN stmt ELSE stmt { (If ($3, $5, $7), rhs 1) }
   | IF LPAREN expr RPAREN stmt %prec NO_ELSE
       { (If ($3, $5, (skip, rhs 5)), rhs 1) }
   | WHILE LPAREN expr RPAREN stmt { (While ($3, $5), rhs 1) }
   | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt
       { (For ($3, $5, $7, $9), rhs 1) }

expr :
   | INT { (Int $1, rhs 1) }
   | VAR { (Var $1, rhs 1) }
   | LPAREN expr RPAREN { (fst $2, rhs 1) }
   | MINUS expr %prec UMINUS { (Binop ((Int 0, rhs 1), Minus, $2), rhs 1) }
   | NOT expr { (Not $2, rhs 1) }
   | expr PLUS expr { (Binop ($1, Plus, $3), rhs 1) }
   | expr MINUS expr { (Binop ($1, Minus, $3), rhs 1) }
   | expr TIMES expr { (Binop ($1, Times, $3), rhs 1) }
   | expr DIVIDE expr { (Binop ($1, Div, $3), rhs 1) }
   | expr EQUAL expr { (Binop ($1, Eq, $3), rhs 1) }
   | expr NEQUAL expr { (Binop ($1, Neq, $3), rhs 1) }
   | expr GREATEREQ expr { (Binop ($1, Gte, $3), rhs 1) }
   | expr GREATER expr { (Binop ($1, Gt, $3), rhs 1) }
   | expr LESSEQ expr { (Binop ($1, Lte, $3), rhs 1) }
   | expr LESS expr { (Binop ($1, Lt, $3), rhs 1) }
   | expr AND expr { (And ($1, $3), rhs 1) }
   | expr OR expr { (Or ($1, $3), rhs 1) }
   | VAR GETS expr { (Assign ($1, $3), rhs 1) }
