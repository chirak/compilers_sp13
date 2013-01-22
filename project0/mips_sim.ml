open Mips_ast
open Byte

exception TODO
exception FatalError

let word = Int32.of_int 4

(* Register file definitions. A register file is a map from a register 
   number to a 32-bit quantity. *)
module IntMap = Map.Make(struct type t = int let compare = compare end)
type regfile = int32 IntMap.t 
let empty_rf = IntMap.empty
let rf_update (r : int) (v : int32) (rf : regfile) : regfile = 
  IntMap.add r v rf
let rf_lookup (r : int) (rf : regfile) : int32 = 
  try IntMap.find r rf with Not_found -> Int32.zero
let string_of_rf (rf : regfile) : string = 
  IntMap.fold (fun key v s -> 
    s^(string_of_int key)^" -> "^(Int32.to_string v)^"\n") rf ""

(* Memory definitions. A memory is a map from 32-bit addresses to bytes. *)
module Int32Map = Map.Make(struct type t = int32 let compare = Int32.compare end)
type memory = byte Int32Map.t
let empty_mem = Int32Map.empty
let mem_update (a : int32) (v : byte) (m : memory) : memory =
  Int32Map.add a v m
let mem_lookup (a : int32) (m : memory) : byte =
  try (Int32Map.find a m) with Not_found -> mk_byte Int32.zero
let string_of_mem (m : memory) : string =
  Int32Map.fold (fun key v s ->
    s^(Int32.to_string key)^" -> "^(Int32.to_string (b2i32 v))^"\n") m ""

(* State *)
type state = { r : regfile; pc : int32; m : memory }

(* Map a program, a list of Mips assembly instructions, down to a starting 
   state. You can start the PC at any address you wish. Just make sure that 
   you put the generated machine code where you started the PC in memory! *)
let rec assem (prog : program) : state =
  let rec assemble (p : program) (mem : memory) (reg : regfile) 
        (start : int32) (curr : int32) =
      match prog with
          [] -> { r = reg; pc = start; m = mem }
        | hd :: tl ->
            let ins = Int32.of_string (Mach.mips_to_mach hd) in
              assemble tl (mem_update curr (mk_byte ins) mem)
                reg start (Int32.add curr word)
  in
  let start_address = Int32.of_string "0x42424242" in
    assemble prog empty_mem empty_rf start_address start_address
;;

(* Given a starting state, simulate the Mips machine code to get a final state *)
let rec interp (init_state : state) : state = raise TODO;;

(*
 MIPS Instruction encoding schemes:

 Register Encoding (Add)
   26-31 (6): opcode
   21-25 (5): source register 1
   16-20 (5): source register 2
   11-15 (5): destination register
   6-10  (5): shift value (for shift instructions only)
   0-5   (6): function value

 Immediate Encoding (Beq, Ori, Lw, Sw)
   26-31 (6): opcode
   21-25 (5): source register 1
   16-20 (5): destination register
   0-15 (16): immediate value

 Load Immediate Encoding (Lui)
   26-31 (6): opcode
   21-25 (5): empty buffer space (0)
   16-20 (5): destination register
   0-15 (16): immediate value
   
 Jump Encoding (Jal)
   26-31 (6): opcode
   0-25 (26): immediate value

 Jump Register Encoding (Jr)
   26-31 (6): opcode
   21-25 (5): destination register
   6-20 (15): empty buffer space (0)
   0-5   (6): function value

 Psuedo Instructions (Li)
   
 *)
