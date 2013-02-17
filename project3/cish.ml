open Ast
open Mips
open Compile

(* This magic is used to glue the generated lexer and parser together.
 * Expect one command-line argument, a file to parse.
 * You do not need to understand this interaction with the system. *)
let parse_file () =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-compile]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Parse.program Lex.lexer (Lexing.from_channel ch)

(* add code to print the result of the program *)
exception NO_MAIN
let rec replace_main (code : Mips.inst list) : Mips.inst list =
  match code with
    | [] -> raise NO_MAIN
    | Label "main" :: is ->
      Label "__main" :: is @
	[Label "main";
	 (* set up frame to call user's main *)
	 Add (R29, R29, Immed (-32l));
	 Sw (R31, R29, 20l);
	 Sw (R30, R29, 16l);
	 Add (R30, R29, Immed 28l);
	 Jal "__main";
	 (* print result *)
	 Add (R4, R2, Reg R0);
	 Li (R2, 1l);
	 Syscall;
	 (* remove frame *)
	 Lw (R31, R29, 20l);
	 Lw (R30, R29, 16l);
	 Add (R29, R29, Immed 32l);
	 Jr R31]
    | i :: is -> i :: replace_main is

let compile_prog prog =
  let compiled = compile prog in
  result2string { code = replace_main compiled.code;
		  data = compiled.data }

(* Usage ./ps3 [file-to-compile] *)
let _ =
  let prog = parse_file() in
  print_string (compile_prog prog) 
