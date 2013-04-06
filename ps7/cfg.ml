open Cfg_ast
exception Implement_Me
exception FatalError

let error (s : string) =
  Printf.printf "%s\n" s;
  raise FatalError
;;

module VarSet =
  Set.Make(struct let compare = Pervasives.compare type t = string end)
;;
let empty_set = VarSet.empty;;

(* used to generate fresh variables *)
let counter = ref 0;;
let new_label() = 
    let c = !counter in
    counter := c+1; "L"^(string_of_int c)
;;

type block_node = 
    { block_label : label;
      instructions : block;
      mutable live_in  : VarSet.t;
      mutable live_out : VarSet.t;
      mutable succ : VarSet.t;
      mutable pred : VarSet.t;
    };;

let new_block_node (l : label) (b : block) : block_node =
  { block_label = l;
    instructions = b;
    live_in  = VarSet.empty;
    live_out = VarSet.empty;
    succ     = VarSet.empty;
    pred     = VarSet.empty;
    gen_set  = VarSet.empty;
  }
;;

let get_block_succ (b : block) =
  let last_inst = List.nth b ((List.length b - 1)) in
    match last_inst with
      | Jump l -> VarSet.add l empty_set
      | If(_,_,_,l1,l2) -> VarSet.add l2 (VarSet.add l1 empty_set)
      | Return -> empty_set
      | _ -> error "Last instruction of block as not a Jump, If or Return"
;;

let rec get_gen_set (b : block) (s : VarSet.t) =
  let get_gen_set' (i : inst) (s : VarSet.t) =
    match i with
        Label _ -> s
      | Move(dest,src) ->
      | Arith (dest, o1, op, o2) ->
      | Load(dest,src,offset) ->
      | Store(dest,offset,src) ->
      | Call op ->
      | Jump l -> 
      | If(_,_,_,_,_) -> s
      (* if x < y then goto L1 else goto L2 *)
      | Return  (* return to caller -- result assumed in R2 *)

  match b with
      [] -> s
    | hd::tl ->
        (match hd with

let build_block_node (b : block) : block_node =
  let node = new_block_node (new_label()) b in
    node.succ <- get_block_succ b;
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

