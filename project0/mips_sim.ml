open Mips_ast
open Utils
open Byte
open Word

exception TODO
exception FatalError

let one_byte   = Int32.one
let four_bytes = 4l

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


(*********************************)
(********* PART 1: ASSEM *********)
(*********************************)


(* Returns list of machine code instructions from the given mips instructions *)
let rec assemble (instructions : inst list) : (string list) =
  let rec assem_instruction instruction =
    match instruction with
        Add (rd, rs, rt) ->
          int_to_bin (6, 0)^
          int_to_bin (5, reg2ind rs)^
          int_to_bin (5, reg2ind rt)^
          int_to_bin (5, reg2ind rd)^
          int_to_bin (5, 0)^
          int_to_bin (6, 32)
      | Beq (rs, rt, offset) ->
          int_to_bin (6, 4)^
          int_to_bin (5, reg2ind rs)^
          int_to_bin (5, reg2ind rt)^
          int32_to_bin (16, offset)
      | Jr rs  ->
          int_to_bin (6, 0)^
          int_to_bin (5, reg2ind rs)^
          int_to_bin (15, 0)^
          int_to_bin (6, 8)
      | Jal target ->
          int_to_bin(2, 6)^
          int32_to_bin (26, target)
      | Li  (rd, imm) ->
          let bin = int32_to_bin(32, imm) in
          let upper_word = bin_to_int32(String.sub bin 0 16) in
          let lower_word = bin_to_int32(String.sub bin 16 16) in
            assem_instruction (Ori (rd, rd, lower_word))^
            assem_instruction (Lui (rd, upper_word))
      | Lui (rt, imm) ->
          int_to_bin (6, 15)^
          int_to_bin (5, 0)^
          int_to_bin (5, reg2ind rt)^
          int32_to_bin (16, imm)
      | Ori (rt, rs, imm) ->
          int_to_bin (6, 13)^
          int_to_bin (5, reg2ind rs)^
          int_to_bin (5, reg2ind rt)^
          int32_to_bin (16, imm)
      | Lw  (rt, rs, offset) -> (* Double check this instruction layout *)
          int_to_bin (6, 35)^
          int_to_bin (5, reg2ind rs)^
          int_to_bin (5, reg2ind rt)^
          int32_to_bin (16, offset)
      | Sw  (rt, rs, offset) -> (* Double check this instruction layout *)
          int_to_bin (6, 43)^
          int_to_bin (5, reg2ind rs)^
          int_to_bin (5, reg2ind rt)^
          int32_to_bin (16, offset)
  in

  let rec assem_instructions (i : inst list) (accum : string list) : string list =
    match i with
        [] -> accum
      | hd :: tl -> 
          let mach = assem_instruction hd in
            if String.length mach > 32 then
              assem_instructions tl ((String.sub mach 0 32) :: (String.sub mach) 32 32 :: accum)
            else
              assem_instructions tl (mach :: accum)
  in
    List.rev (assem_instructions instructions [])
;;

(* Puts the given list of bytes at the given memory address *)
let rec put_bytes bytes mem addr =
match bytes with
    [] -> mem
  | hd :: tl -> put_bytes tl (mem_update addr hd mem) (Int32.add addr one_byte)
;;

(* Loads a four byte word from memory at the given address *)
let load_word (mem : memory) (addr : int32) : word =
  let bytes =
    (mem_lookup addr mem) ::
    (mem_lookup (Int32.add addr 1l) mem) ::
    (mem_lookup (Int32.add addr 2l) mem) ::
    (mem_lookup (Int32.add addr 3l) mem) :: []
  in
    mk_word_from_bytes bytes
;;

(* Map a program, a list of Mips assembly instructions, down to a starting 
   state. You can start the PC at any address you wish. Just make sure that 
   you put the generated machine code where you started the PC in memory! *)
let rec assem (prog : program) : state =

  (* Lays out program in memeory *)
  let rec load_prog (mach : word list) (mem : memory) (addr : int32) =
      match mach with
          (* Append program exit instruction to end *)
          [] -> put_bytes (split_word (mk_word Int32.zero)) mem addr
        | hd :: tl ->
            let ins = split_word hd in
              load_prog tl (put_bytes ins mem addr) (Int32.add addr four_bytes)
  in

  let machine_code = List.map mk_word_from_bin (assemble prog) in
  let start_address = Int32.of_string "0x42424242" in
  let mem = load_prog machine_code empty_mem start_address in
    { r = empty_rf; pc = start_address; m = mem }
;;

(**********************************)
(********* PART 2: INTERP *********)
(**********************************)


(* All of interpreter's mips instructions *)
let add (instr : word) (s : state) : state =
  let rs_val = rf_lookup (get_rs instr) s.r in
  let rt_val = rf_lookup (get_rt instr) s.r in
  let result = Int32.add rs_val rt_val in
  let new_reg = rf_update (get_rd instr) result s.r in
    { r = new_reg; pc = (Int32.add s.pc 4l); m = s.m }
;;

let beq (instr : word) (s : state) : state =
  let rs_val = rf_lookup (get_rs instr) s.r in
  let rt_val = rf_lookup (get_rt instr) s.r in
  let new_pc =
    if rs_val = rt_val then
      Int32.add (Int32.of_int ((get_offset instr) * 4)) s.pc
    else
      Int32.add 4l s.pc
  in
    { r = s.r; pc = new_pc; m = s.m }
;;

let jr (instr : word) (s : state) : state =
  let rs_val = rf_lookup (get_rs instr) s.r in
  let new_pc = Int32.add rs_val s.pc in
    { r = s.r; pc = new_pc; m = s.m }
;;

let jal (instr : word) (s : state) : state =
  let return_address = Int32.add s.pc 4l in
  let jump_address = Int32.of_int (get_address instr) in
  let new_reg = rf_update (reg2ind R31) return_address s.r in
    { r = new_reg; pc = jump_address; m = s.m }
;;

let lui (instr : word) (s : state) : state =
  let imm_bin = int_to_bin (16, get_immed instr) in
  let imm_dec = Int32.of_string ("0b"^imm_bin^"0000000000000000") in
  let new_reg = rf_update (get_rt instr) imm_dec s.r in
    { r = new_reg; pc = (Int32.add s.pc 4l); m = s.m }
;;

let ori (instr : word) (s : state) : state =
  let rs_val = rf_lookup (get_rs instr) s.r in
  let imm_val = Int32.of_int (get_immed instr) in
  let result = Int32.logor rs_val imm_val in
  let new_reg = rf_update (get_rt instr) result s.r in
    { r = new_reg; pc = (Int32.add s.pc 4l); m = s.m }
;;

let lw (instr : word) (s : state) : state =
  let rs_val = rf_lookup (get_rs instr) s.r in
  let off_val = Int32.of_int(get_offset instr) in
  let mem_addr = Int32.add rs_val off_val in
  let mem_word = load_word s.m mem_addr in
  let new_reg = rf_update (get_rt instr) mem_word.w s.r in
    { r = new_reg; pc = (Int32.add s.pc 4l); m = s.m }
;;

let sw (instr : word) (s : state) : state =
  let rt_val = rf_lookup (get_rt instr) s.r in
  let rs_val = rf_lookup (get_rs instr) s.r in
  let offset_val = Int32.of_int (get_offset instr) in
  let mem_addr = Int32.add rs_val offset_val in
  let bytes = split_word (mk_word rt_val) in
  let new_mem = put_bytes bytes s.m mem_addr in
    { r = s.r; pc = (Int32.add s.pc 4l); m = new_mem }
;;

(* Returns operation corresponding to given machine code instruction *)
let get_op (instr : word) =
  let op_code = get_opcode instr in
  let func_code = get_func instr in
    match op_code with
        0  -> 
          if func_code = 8 then
            jr
          else if func_code = 32 then
            add
          else
            raise FatalError
      | 3  -> jal | 4  -> beq
      | 13 -> ori | 15 -> lui
      | 35 -> lw  | 43 -> sw
      | _  -> raise FatalError
;;

(* Given a starting state, simulate the Mips machine code to get a final state *)
let rec interp (init_state : state) : state =
  let w = load_word init_state.m init_state.pc in
    Printf.printf "address: %lx\n" init_state.pc;
    if w.w = Int32.zero then
      init_state
    else
      let new_state = (get_op w) w init_state in
        interp new_state
;;

