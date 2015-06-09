{
open Parse
open Lexing

let incr_lineno lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }

let depth = ref 0 
let inc r = r := !r + 1
let dec r = r := !r - 1

let println str =
    Printf.printf "%s\n" str
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
  | digit+ { println "INT"; INT(int_of_string(Lexing.lexeme lexbuf)) } 
  | '-'digit+ { INT(int_of_string(Lexing.lexeme lexbuf)) } 
  | "/*" { inc depth; comment lexbuf }
  | "=" { println "EQUAL"; EQUAL }
  | ";" { println "SEMI"; SEMI }
  | "return" { println "RETURN"; RETURN }
  | "if" { println "IF"; IF }
  | "else" { println "ELSE"; ELSE }
  | "for" { println "FOR"; FOR }
  | "while" { println "WHILE"; WHILE }
  | "(" { println "LPAREN"; LPAREN }
  | ")" { println "RPAREN"; RPAREN }
  | "{" { println "LBRACE"; LBRACE }
  | "}" { println "RBRACE"; RBRACE }
  | "+" { println "PLUS"; PLUS }
  | "-" { println "MINUS"; MINUS }
  | "*" { println "TIMES"; TIMES }
  | "/" { println "DIVIDE"; DIVIDE }
  | ">" { println "GT"; GT }
  | ">=" { println "GTE"; GTE }
  | "<" { println "LT"; LT }
  | "<=" { println "LTE"; LTE }
  | "==" { println "EQ"; EQ }
  | "!=" { println "NEQ"; NEQ }
  | "&&" { println "AND"; AND }
  | "||" { println "OR"; OR }
  | "!" { println "NOT"; NOT }
  | ch+ { println "VAR"; VAR(Lexing.lexeme lexbuf) }
  | eof { EOF }

and comment = parse
  | "*/" { dec depth ; if !depth = 0 then lexer lexbuf else comment lexbuf }
  | "/*" { inc depth ; comment lexbuf }
  | eof  { raise (Failure "missing comment terminator") }
  | _    { comment lexbuf }

