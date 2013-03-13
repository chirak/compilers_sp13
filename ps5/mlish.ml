open Mlish_ast

(* This magic is used to glue the generated lexer and parser together.
 * Expect one command-line argument, a file to parse.
 * You do not need to understand this interaction with the system. *)
let parse_file () =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Ml_parse.program Ml_lex.lexer (Lexing.from_channel ch)

let compile_prog prog = 
  let _ = Mlish_type_check.type_check_exp prog in
  Mlish_compile.compile_exp prog

let run_prog prog = Scish_eval.run prog

let dump p = print_string (Scish_ast.exp2string p)

let _ = 
  let prog = parse_file() in
  let prog' = compile_prog prog in
  dump prog'
