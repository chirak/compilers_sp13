(* Needs work... *)
let fetch_word mem addr = 
  let byte_to_bin b =
    Mach.int32_to_bin (8, (b2i32 b))
  in

  let rec fetch m a n accum =
    if n < 0 then
      accum
    else
      let byte = mem_lookup (Int32.add a (Int32.of_int n)) m in
        fetch m a (n-1) accum^(byte_to_bin byte)
  in
    fetch mem addr 3 ""
;;


let interp_word instr state =
  let op_code = Int32.to_int (Int32.of_string ("0b"^(String.sub instr 0 6))) in
    match op_code with
        0  -> 
          let func_code = 
            Int32.to_int (Int32.of_string ("0b"^(String.sub instr 25 6)))
          in
            match func_code with
                8  -> jr instr state
              | 32 -> add instr state
      | 3  -> jal instr state
      | 4  -> beq instr state
      | 13 -> ori instr state
      | 15 -> lui instr state
      | 35 -> lw  instr state
      | 43 -> sw  instr state
      | _  -> raise FatalError;
;;

let get_int32 s start len =
  let bin = String.sub s start len in
    Int32.of_string ("0b"^bin)
;;

let get_int s start len =
    Int32.to_int (get_int32 s start len)
;;


(* Does register update, returns unit *)
let add instr regs =
  let rs_val = rf_lookup (get_int instr 6 5) regs in
  let rt_val = rf_lookup (get_int instr 11 5) regs in
  let result = Int32.add rs_val rt_val in
    rf_update (get_int instr 16 5) result
;;

(* For now returns offset *)
let beq instr regs =
  let rs_val = rf_lookup (get_int instr 6 5) regs in
  let rt_val = rf_lookup (get_int instr 11 5) regs in
    if rs_val = rt_val then
      get_int32 instr 16 16
    else
      Int32.of_int 4
;;

(* Returns new PC memory adderss *)
let jr instr =
  rf_lookup (get_int instr 6 5) regs
;;

(* Returns new PC memory adderss *)
let jal instr =
  get_int32 instr 6 26
;;

(* Does register update, returns unit *)
let lui instr regs =
  let imm_bin = String.sub instr 16 16 in
  let imm_dec = Int32.of_string ("0b"^imm^"0000000000000000") in
    rf_update (get_int instr 11 5) imm_dec regs
;;

(* Does register update, returns unit *)
let ori instr regs =
  let rs_val = rf_lookup (get_int instr 6 5) regs in
  let imm_val = (get_int32 instr 16 16) regs in
  let result = Int32.logor rs_val imm_val in
    rf_update (get_int instr 11 5) result regs
;;

(* Does register update, returns unit *)
let lw instr regs =
  let rs_val = rf_lookup (get_int instr 6 5) regs in
  let off_val = get_int32 instr 16 16 in
  let address = Int32.add rs_val off_val in
    rf_update (get_int instr 11 5) address regs
;;

(* Updates memory, and returns unit *)
let sw instr regs mem =
  let rt_val = rf_lookup (get_int instr 11 5) regs in
  let rs_val = rf_lookup (get_int instr 6 5) regs in
  let off_val = get_int32 instr 16 16 in
  let word = Mach.int32_to_bin (32, rt_val) in
  let address = Int32.add rs_val off_val in
  let bytes = [] in

  let rec store_bytes b m addr =
    match b with
        [] -> m
      | hd :: tl -> put_word tl (mem_update addr hd m) (Int32.add addr Int32.one)
  in
    store_bytes bytes mem address
;;
