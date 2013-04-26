open Cfg
open Interfere_graph
open Reg_alloc
open Mips

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

(* add code to print the result of the program *)
exception NO_MAIN
let rec replace_main (code : Mips.inst list) : Mips.inst list =
  match code with
    | [] -> raise NO_MAIN
    | Label "main" :: is ->
        Label "__main" :: is @
        [Label "main";
         (* set up frame to call user's main *)
         Add (R29, R29, Immed (-256l));
         Sw (R31, R29, 252l);
         Sw (R30, R29, 248l);
         Add (R30, R29, Immed 252l);
         Jal "__main";
         (* print result *)
         Add (R4, R2, Reg R0);
         Li (R2, 1l);
         Syscall;
         (* remove frame *)
         Lw (R31, R29, 252l);
         Lw (R30, R29, 248l);
         Add (R29, R29, Immed 256l);
         Jr R31]
    | i :: is -> i :: replace_main is

let mips_header =
      "\t.text\n"       ^
      "\t.align\t2\n"   ^
      "\t.globl main\n"

let mips_footer = 
      "\n\n"        ^
      "\t.data\n"   ^
      "\t.align 0\n"

let _ =
  let debug = false in 
  let prog = parse_file() in
  let cfg_prog = List.map Cfg_ast.fn2blocks prog in
  let reg_alloc_prog = List.map Reg_alloc.reg_alloc cfg_prog in
  let mips_prog = List.concat (List.map cfg_to_mips reg_alloc_prog) in

    (* print out input cish program, cfg program, and reg allocated cfg
     * program if debug flag is enabled*)
    if debug then (
      print_string (Cish_ast.prog2string prog);
      print_string (Cfg_ast.prog2string reg_alloc_prog);
      print_string (Cfg_ast.prog2string reg_alloc_prog);
    );
    (* Print out final mips program *)
    print_string mips_header;
    List.iter println (List.map inst2string mips_prog);
    print_string mips_footer;

