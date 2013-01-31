(* You need to add new definition to build the
 * appropriate terminals to feed to parse.mly.
 *)

{
open Parse
open Lexing

let incr_lineno lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }
}

(* definition section *)
let cr='\013'
let nl='\010'
let eol=(cr nl|nl|cr)
let ws=('\012'|'\t'|' ')*
let digit=['0'-'9'] 
let ch = ['A'-'Z''a'-'z''_''.']['.''a'-'z''A'-'Z''0'-'9''_']*

(* rules section *)
rule lexer = parse

| eol { incr_lineno lexbuf; lexer lexbuf } 
| ws+ { lexer lexbuf }
| "=" { EQ }
| ";" { SEMI }
| "return" { RETURN }
| "for" { FOR }
| "while" { WHILE }
| "(" { LPAREN }
| ")" { RPAREN }
| "{" { LBRACE }
| "}" { RBRACE }
| "+" { PLUS }
| "-" { MINUS }
| "*" { TIMES }
| "/" { DIVIDE }
| ">" { GT }
| ">=" { GTE }
| "<" { LT }
| "<=" { LTE }
| "==" { EQ }
| "!=" { NEQ }
| "&&" { AND }
| "||" { OR }
| "!" { NOT }
| digit+ { INT(int_of_string(Lexing.lexeme lexbuf)) } 
| '-'digit+ { INT(int_of_string(Lexing.lexeme lexbuf)) } 
| ch+ { VAR(Lexing.lexeme lexbuf) }
| eof { EOF }

