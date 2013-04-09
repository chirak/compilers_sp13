open Cfg
open Interfere_graph
(* This magic is used to glue the generated lexer and parser together.
 * Expect one command-line argument, a file to parse.
 * You do not need to understand this interaction with the system. *)
let parse_file() =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Cish_parse.program Cish_lex.lexer (Lexing.from_channel ch)

let parse_stdin() = 
  Cish_parse.program Cish_lex.lexer (Lexing.from_channel stdin)

let _ =
  let prog = parse_file() in
  let cfg_functions = List.map Cfg_ast.fn2blocks prog in
  let cfgs = List.map build_cfg cfg_functions in
  (* let cfg_prog = Cfg_ast.prog2string functions in *)
  let igraphs = List.map build_interfere_graph cfgs in
    List.iter (fun g -> print_graph g; print_string "\n\n";) igraphs

