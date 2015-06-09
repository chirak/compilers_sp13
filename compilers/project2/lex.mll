(* Lexer for Fish *)

{
open Parse
open Lexing

let incr_lineno lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }

exception UnrecognizedCharacter
}

(* definition section *)
let cr='\013'
let nl='\010'
let eol=(cr nl|nl|cr)
let ws=('\012'|'\t'|' ')*
let digit=['0'-'9']
let intlit = '-'? digit+
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*

(* rules section *)
rule lexer = parse
| eol { incr_lineno lexbuf; lexer lexbuf } 
| ws+ { lexer lexbuf }
| "/*" { comment lexbuf }
| intlit as n { INT (int_of_string n) } 
| "return" { RETURN }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "for" { FOR }
| id as x { VAR x }
| ';' { SEMI }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LBRACE }
| '}' { RBRACE }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| "==" { EQUAL }
| "!=" { NEQUAL }
| ">=" { GREATEREQ }
| '>' { GREATER }
| "<=" { LESSEQ }
| '<' { LESS }
| '!' { NOT }
| "&&" { AND }
| "||" { OR }
| '=' { GETS }
| eof { EOF }
| _ { raise UnrecognizedCharacter }

and comment = parse
    | eol { incr_lineno lexbuf; comment lexbuf }
    | "*/" { lexer lexbuf }
    | _ { comment lexbuf }
