open Cfg_ast
exception Implement_Me
exception FatalError

(* Utility functions *)
let println (s : string) =
  Printf.printf "%s\n" s

let error (s : string) =
  Printf.printf "%s\n" s;
  raise FatalError

(* Set which Stores operands of type (Var of string) and (Reg of Mip.reg)
 * Used for creating gen/kill sets for single instructions and blocks.
 * Also used in liveliness analysis.
 *)
module VarSet =
  Set.Make(struct let compare = Pervasives.compare type t = operand end)
let empty_set = VarSet.empty;;
let single (o : operand) = VarSet.add o empty_set;;

(* Builds a VarSet from a list of operands. Only Vars and Regs are added *)
let set_vars (ops : operand list) : VarSet.t =
  List.fold_left (fun s o -> match o with
                      | Var _ | Reg _ -> VarSet.add o s 
                      | _ -> s) empty_set ops

let vs2string (set : VarSet.t) =
  let vars = VarSet.fold (fun s1 s2 -> (op2string s1)^" "^s2) set "" in
    Printf.sprintf "%s" vars

(* General purpose string set *)
module StringSet =
  Set.Make(struct let compare = Pervasives.compare type t = string end)

(* Record thats holds gen and kill set for a single instruction *)
type inst_set = 
  { 
    i : inst;
    igen_set : VarSet.t;
    ikill_set : VarSet.t;
  }

let instset2string (i : inst_set) : string =
  let inst_str = inst2string i.i in
  let gen_str  = "[Gen]"^(vs2string i.igen_set) in
  let kill_str = "[Kill]"^(vs2string i.ikill_set) in
    Printf.sprintf "%s\n\t%s\n\t%s" inst_str gen_str kill_str

(* Produces a gen set and kill set for a single instruction *)
(*
Statement               Gen     Kill             
x:=y                    {y}      {x}
x:=p(y,z)               {y,z}    {x}
x:=*(y+i)               {y}      {x}
*(v+i):=x               {x}      { }
Call f                  {f}      {x}
*)
let rec generate_inst_set (i : inst) : inst_set =
  let (gen_set, kill_set) = 
    match i with
      | Move(dest,src)          -> (set_vars [src], set_vars[dest])
      | Arith (dest, o1, _, o2) -> (set_vars [o1;o2], set_vars [dest])
      | Load(dest,src,_)        -> (set_vars [src], set_vars [dest])
      | Store(dest,_,src)       -> (set_vars[src], empty_set)
      | Call f                  -> (set_vars[f], empty_set)
      | _                       -> (empty_set, empty_set)
  in
    { i = i; igen_set = gen_set; ikill_set = kill_set }

(* Record thats holds gen and kill sets for a single block
 * Also holds gen and kill sets for each instruction in block *)
type block_set = 
  {
    insts : inst_set list;
    bgen_set : VarSet.t;
    bkill_set: VarSet.t; 
  }

let blockset2string (b : block_set) (label : string) : string =
  let gen_str  = "[Gen]"^(vs2string b.bgen_set) in
  let kill_str = "[Kill]"^(vs2string b.bkill_set) in
    Printf.sprintf "\t%s\n\t%s" gen_str kill_str

(* Produces gen and kill sets for a single block *)
let gen_block_set (b : block) : block_set =
  let rec gen (inst_sets : inst_set list) : VarSet.t =
    match inst_sets with
      | [] -> error "Block does not contain instructions"
      | e::[] ->
          (match e.i with
             | Return | Jump _ -> empty_set
             | If(o1,_,o2,_,_) -> set_vars [o1;o2]
             | _ -> error "Last inst in block must be Return, Jump, or If")
      | hd::tl ->
          (VarSet.union (hd.igen_set) (VarSet.diff (gen tl) hd.ikill_set))
  in

  let rec kill (inst_sets : inst_set list) : VarSet.t =
    List.fold_left (fun a is -> VarSet.union is.ikill_set a) empty_set inst_sets  
  in

  let inst_sets = List.map generate_inst_set b in
  let block_gen_set = gen inst_sets in
  let block_kill_set = kill inst_sets in
    { insts = inst_sets; bgen_set = block_gen_set; bkill_set = block_kill_set }


(* Single node of Control Flow Graph *)
type block_node = 
    {
      block_label : label;
      mutable gen_kill_sets : block_set;
      mutable live_in  : VarSet.t;
      mutable live_out : VarSet.t;
      mutable succ : StringSet.t; (* Set of block labels *)
    }

let new_block_node (l : label) : block_node =
  { 
    block_label = l;
    gen_kill_sets = { insts = []; bgen_set = empty_set; bkill_set = empty_set };
    live_in  = VarSet.empty;
    live_out = VarSet.empty;
    succ     = StringSet.empty;
  }

(* Determines the outgoing edges of a single block by inspecting the last
 * instruction of the block *)
let get_block_succ (b : block) : StringSet.t =
  let last_inst = List.nth b ((List.length b - 1)) in
    match last_inst with
      | Jump l -> StringSet.add l StringSet.empty
      | If(_,_,_,l1,l2) -> StringSet.add l2 (StringSet.add l1 StringSet.empty)
      | Return -> StringSet.empty
      | _ -> error "Last instruction of block was not a Jump, If, or Return"

(* Builds a single block node with initialized gen/kill sets, live in vars, and
 * successor nodes (outgoing edges)
 *)
let build_block_node (b : block) : block_node =
  let label =
    match b with
    | (Label(l))::_ -> l
    | _             -> error "Block does not start with a label."
  in
  let node = new_block_node label in
  let successors = get_block_succ b in
  let live_in = gen_block_set b in
    node.succ <- successors;
    node.live_in <- live_in.bgen_set;
    node.gen_kill_sets <- live_in;
    node

module StringMap = 
  Map.Make(struct let compare = Pervasives.compare type t = string end)
type cfg = block_node StringMap.t
let empty_cfg = StringMap.empty

(* Build a Control Flow Graph for single function *)
let build_cfg (f : func) : cfg =
  (* Builds our initial CFG with gen/kill sets for instructions + blocks and
   * initial live_in sets for each node_block *)
  let base_cfg = 
    List.fold_left 
      (fun a b -> 
         let block_node = build_block_node b in 
           StringMap.add block_node.block_label block_node a)
      empty_cfg f
  in

  (* Flag for keeping track of whether any changes have been made to the CFG
   * if none have been made, we can exit and return the CFG *)
  let changed = ref true in
  let build (block : block_node) =
    let out = StringSet.fold 
                (fun b a -> 
                   VarSet.union (StringMap.find b base_cfg).live_in a) 
                block.succ empty_set
    in
      if not (VarSet.equal out block.live_out) then
        (block.live_out <- out; 
         block.live_in  <- (VarSet.union (block.gen_kill_sets.bgen_set)
                             (VarSet.diff out (block.gen_kill_sets.bkill_set)));
         changed := true;)
  in
    (* Loop until no changes occur in CFG *)
    (while !changed do
       changed := false;
       StringMap.iter (fun _ -> build) base_cfg;
     done);
    base_cfg

(* an interference graph maps a variable x to the set of variables
 * y such that x and y are live at the same point in time.  It's up to
 * you how you want to represent the graph.  I've just put in a dummy
 * definition for now.  *)


(*******************************************************************)
(* PS8 TODO:  graph-coloring, coalescing register assignment *)
(* You will need to build a mapping from variables to MIPS registers
   using the ideas behind the graph-coloring register allocation
   heuristics described in class.  This may involve spilling some
   of the variables into memory, so be sure to adjust the prelude
   of the function so that you allocate enough space on the stack
   to store any spilled variables.  The output should be a CFG
   function that doesn't use any variables (except for function
   names.)
*)
let reg_alloc (f : func) : func = 
    raise Implement_Me

(* Finally, translate the ouptut of reg_alloc to Mips instructions *)
let cfg_to_mips (f : func ) : Mips.inst list = 
    raise Implement_Me

