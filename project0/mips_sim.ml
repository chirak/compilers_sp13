open Mips_ast
open Utils
open Byte

exception TODO
exception FatalError

let one_byte   = Int32.one
let four_bytes = Int32.of_int 4

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

(****************************************************)

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
          int_to_bin (6, 35)^
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

(* Map a program, a list of Mips assembly instructions, down to a starting 
   state. You can start the PC at any address you wish. Just make sure that 
   you put the generated machine code where you started the PC in memory! *)
let rec assem (prog : program) : state =
  let split_word str =
    let b_one   = bin2b (String.sub str 0 8) in
    let b_two   = bin2b (String.sub str 8 8) in
    let b_three = bin2b (String.sub str 16 8) in
    let b_four  = bin2b (String.sub str 24 8) in
      b_one :: b_two :: b_three :: b_four :: []
  in

  let rec put_word bytes mem addr =
    match bytes with
        [] -> mem
      | hd :: tl -> put_word tl (mem_update addr hd mem) (Int32.add addr one_byte)
  in

  let rec load_prog (mach : string list) (mem : memory) (addr : int32) =
      match mach with
          [] -> mem
        | hd :: tl ->
            let ins = split_word hd in
              load_prog tl (put_word ins mem addr) (Int32.add addr four_bytes)
  in

  let machine_code = assemble prog in
  let start_address = Int32.of_string "0x42424242" in
  let mem = load_prog machine_code empty_mem start_address in
    { r = empty_rf; pc = start_address; m = mem }
;;

(****************************************************)



(* Given a starting state, simulate the Mips machine code to get a final state *)
let rec interp (init_state : state) : state = raise TODO;;




















