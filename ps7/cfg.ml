open Cfg_ast
exception Implement_Me
exception FatalError

let error (s : string) =
  Printf.printf "%s\n" s;
  raise FatalError
;;

(* used to generate fresh block labels *)
let counter = ref 0;;
let new_label() = 
    let c = !counter in
    counter := c+1; "L"^(string_of_int c)
;;

module VarSet =
  Set.Make(struct let compare = Pervasives.compare type t = string end)
;;
let empty_set = VarSet.empty;;
let single (v : var) = VarSet.add v empty_set;;
(* Builds a VarSet from a list of operands. Only Vars are added *)
let set_vars (ops : operand list) : VarSet.t =
  List.fold_left (fun s o ->
                    match o with 
                      | Var x -> VarSet.add x s 
                      | _ -> s) empty_set ops
;;

(* Gen and kill set for a single instruction *)
type inst_set = { i : inst; igen_set : VarSet.t; ikill_set : VarSet.t; };;

(* Produces a gen set and kill set for a single instruction *)
let rec gen_inst_set (i : inst) : inst_set =
  let (gen_set, kill_set) = 
    match i with
      | Move(dest,src) -> (set_vars [src], set_vars[dest])
      | Arith (dest, o1, _, o2) -> (set_vars [o1;o2], set_vars [dest])
      | Load(dest,src,_) -> (set_vars [src], set_vars [dest])
      | Store(dest,_,src) -> (set_vars[src], empty_set)
      | Call f -> (set_vars[f], empty_set)
      | _ -> (empty_set, empty_set)
  in
    {i = i; igen_set = gen_set; ikill_set = kill_set}
;;

(* Produces gen and kill sets for a single block *)
type block_set = { b : block; bgen_set : VarSet.t; bkill_set: VarSet.t; };;
let gen_block_set (b : block) : block_set =

  let rec gen (inst_sets : inst_set list) : VarSet.t =
    match inst_sets with
      | [] -> error "Block does not contain instructions"
      | e::[] ->
          (match e.i with
               (Return | Jump _) -> empty_set
             | If(o1,_,o2,_,_) -> set_vars [o1;o2]
             | _ -> error "Last instruction in block must be Return, Jump, or If")
      | hd::tl ->
          (VarSet.union (hd.igen_set) (VarSet.diff (gen tl) hd.ikill_set))
  in

  let rec kill (inst_sets : inst_set list) : VarSet.t =
    match inst_sets with
        [] -> empty_set
      | hd::tl -> VarSet.union hd.ikill_set (kill tl)
  in

  let inst_sets = List.map gen_inst_set b in
  let block_gen_set = gen inst_sets in
  let block_kill_set = kill inst_sets in
    {b = b; bgen_set = block_gen_set; bkill_set = block_kill_set}
;;

(* may have to add more fields for register coalescing *)
type block_node = 
    { block_label : label;
      instructions : block;
      mutable live_in  : VarSet.t;
      mutable live_out : VarSet.t;
      mutable succ : VarSet.t;
      (* mutable pred : VarSet.t; *)
    };;

let new_block_node (l : label) (b : block) : block_node =
  { block_label = l;
    instructions = b;
    live_in  = VarSet.empty;
    live_out = VarSet.empty;
    succ     = VarSet.empty;
    (* pred     = VarSet.empty; *)
  }
;;

let get_block_succ (b : block) =
  let last_inst = List.nth b ((List.length b - 1)) in
    match last_inst with
      | Jump l -> VarSet.add l empty_set
      | If(_,_,_,l1,l2) -> VarSet.add l2 (VarSet.add l1 empty_set)
      | Return -> empty_set
      | _ -> error "Last instruction of block as not a Jump, If, or Return"
;;


let build_block_node (b : block) : block_node =
  let node = new_block_node (new_label()) b in
  let successors = get_block_succ b in
  let live_out = gen_block_set b in
    node.succ <- successors;
    node.live_out <- live_out.bgen_set;
    node
;;


(* an interference graph maps a variable x to the set of variables
 * y such that x and y are live at the same point in time.  It's up to
 * you how you want to represent the graph.  I've just put in a dummy
 * definition for now.  *)
type interfere_graph = unit

(* given a function (i.e., list of basic blocks), construct the
 * interference graph for that function.  This will require that
 * you build a dataflow analysis for calculating what set of variables
 * are live-in and live-out for each program point. *)
let build_interfere_graph (f : func) : interfere_graph = 
    raise Implement_Me

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

