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
  Set.Make(struct let compare = Pervasives.compare type t = operand end)
;;

module StringSet =
  Set.Make(struct let compare = Pervasives.compare type t = string end)
;;

let empty_set = VarSet.empty;;
let single (o : operand) = VarSet.add o empty_set;;
(* Builds a VarSet from a list of operands. Only Vars are added *)
let set_vars (ops : operand list) : VarSet.t =
  List.fold_left (fun s o -> match o with
                      | Var _ | Reg _ -> VarSet.add o s 
                      | _ -> s) empty_set ops
;;

let varset2string (set : VarSet.t) (label : string) =
  let vars = VarSet.fold (fun s1 s2 -> (op2string s1)^" "^s2) set "" in
    Printf.sprintf "%s: %s" label vars
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
    { i = i; igen_set = gen_set; ikill_set = kill_set }
;;

let instset2string (i : inst_set) : string =
  let inst_str = inst2string i.i in
  let gen_str  = varset2string i.igen_set "[Gen]" in
  let kill_str = varset2string i.ikill_set "[Kill]" in
    Printf.sprintf "%s\n\t%s\n\t%s" inst_str gen_str kill_str
;;

(* Produces gen and kill sets for a single block *)
type block_set = { insts : inst_set list; bgen_set : VarSet.t; bkill_set: VarSet.t; };;
let gen_block_set (b : block) : block_set =

  let rec gen (inst_sets : inst_set list) : VarSet.t =
    match inst_sets with
      | [] -> error "Block does not contain instructions"
      | e::[] ->
          (match e.i with
             | Return | Jump _ -> empty_set
             | If(o1,_,o2,_,_) -> set_vars [o1;o2]
             | _ -> error "Last instruction in block must be Return, Jump, or If")
      | hd::tl ->
          (VarSet.union (hd.igen_set) (VarSet.diff (gen tl) hd.ikill_set))
  in

  let rec kill (inst_sets : inst_set list) : VarSet.t =
    List.fold_left (fun a is -> VarSet.union is.ikill_set a) empty_set inst_sets  
  in

  let inst_sets = List.map gen_inst_set b in
  let block_gen_set = gen inst_sets in
  let block_kill_set = kill inst_sets in
    { insts = inst_sets; bgen_set = block_gen_set; bkill_set = block_kill_set }
;;

let blockset2string (b : block_set) (label : string) : string =
  let gen_str  = varset2string b.bgen_set "[Gen]" in
  let kill_str = varset2string b.bkill_set "[Kill]" in
    Printf.sprintf "\t%s\n\t%s" gen_str kill_str
;;

(* may have to add more fields for register coalescing *)
type block_node = 
    { block_label : label;
      instructions : block;
      mutable gen_kill_sets : block_set;
      mutable live_in  : VarSet.t;
      mutable live_out : VarSet.t;
      mutable succ : StringSet.t;
      (* mutable pred : VarSet.t; *)
    };;

let new_block_node (l : label) (b : block) : block_node =
  { block_label = l;
    instructions = b;
    gen_kill_sets = { insts = []; bgen_set = empty_set; bkill_set = empty_set };
    live_in  = VarSet.empty;
    live_out = VarSet.empty;
    succ     = StringSet.empty;
    (* pred     = VarSet.empty; *)
  }
;;

let get_block_succ (b : block) : StringSet.t =
  let last_inst = List.nth b ((List.length b - 1)) in
    match last_inst with
      | Jump l -> StringSet.add l StringSet.empty
      | If(_,_,_,l1,l2) -> StringSet.add l2 (StringSet.add l1 StringSet.empty)
      | Return -> StringSet.empty
      | _ -> error "Last instruction of block was not a Jump, If, or Return"
;;

let build_block_node (b : block) : block_node =
  let label =
    match b with
    | (Label(l))::_ -> l
    | _             -> error "Block does not start with a label."
  in
  let node = new_block_node label b in
  let successors = get_block_succ b in
  let live_in = gen_block_set b in
    node.succ <- successors;
    node.live_in <- live_in.bgen_set;
    node.gen_kill_sets <- live_in;
    node
;;

module StringMap = 
  Map.Make(struct let compare = Pervasives.compare type t = string end)
;;

let empty_cfg = StringMap.empty;;
let single_cfg k v = StringMap.add k v empty_cfg;;

let build_cfg (f : func) =
    
  let base_cfg = 
    List.fold_left 
      (fun a b -> 
         let block_node = build_block_node b in 
           StringMap.add block_node.block_label block_node a)
      empty_cfg
      f
  in

  let changed = ref true in

  (* returns true if build does not mutate *)
  let build (block : block_node) =
    let out = StringSet.fold 
                (fun b a -> 
                   VarSet.union (StringMap.find b base_cfg).live_in a) 
                block.succ empty_set
    in
      (* print_string (varset2string out ""); *)
      (* print_string "\n"; *)
      (* print_string (varset2string block.live_out ""); *)
      (* print_string "\n"; *)
      if not (VarSet.equal out block.live_out) then
        (block.live_out <- out; 
         block.live_in <- VarSet.union block.gen_kill_sets.bgen_set (VarSet.diff out block.gen_kill_sets.bkill_set);
         changed := true;)
  in

    (while !changed do
       changed := false;
       StringMap.iter (fun _ -> build) base_cfg;
     done);

    base_cfg
;;

module OpMap = 
  Map.Make(struct let compare = Pervasives.compare type t = operand end)
;;

(* an interference graph maps a variable x to the set of variables
 * y such that x and y are live at the same point in time.  It's up to
 * you how you want to represent the graph.  I've just put in a dummy
 * definition for now.  *)
type interfere_graph = VarSet.t OpMap.t;;

let print_graph (i : interfere_graph) =
  OpMap.iter (fun k v ->
                    let nodes = varset2string v "" in
                      Printf.printf "%s -> %s\n" (op2string k) nodes) i
;;


(* given a function (i.e., list of basic blocks), construct the
 * interference graph for that function.  This will require that
 * you build a dataflow analysis for calculating what set of variables
 * are live-in and live-out for each program point. *)
let build_interfere_graph (f : func) : interfere_graph =
  let cfg = build_cfg f in
  
  let rec fold_insts (live_set : VarSet.t) (g : interfere_graph) = function
    | { i = _; igen_set = gen; ikill_set = kill; }::tl ->
      (* Remove killed variables *)
      let live' = VarSet.diff live_set kill in
      (* Add edges from live_set nodes to gen set nodes *)
      print_string "ENTERING G'\n";
      let g' =
        VarSet.fold
          (fun x a -> 
            Printf.printf "Finding %s for g'\n" (op2string x);
            let connectors = OpMap.find x a in
            let connectors' = VarSet.union gen connectors in
            OpMap.add x connectors' a)
          live_set
          g
      in
      print_string "EXITING G'\n";
      (* Add edges from gen set nodes to live_set nodes *)
      let g'' = 
        VarSet.fold 
          (fun x a -> 
            print_string "\nENTERING G''\n";
            Printf.printf "Finding %s for g''\n" (op2string x);
            if OpMap.mem x a then
              OpMap.add x (VarSet.union (OpMap.find x a) live_set) a
            else
              OpMap.add x live_set a)
          gen
          g'
      in
      (* Add gen set to live set *)
      let live'' = VarSet.union gen live' in
      (* Recur *)
      fold_insts live'' g'' tl
    | [] -> g
  in
  
  let inter_build_block (b : block_node) (g : interfere_graph) =
    let live_set = b.live_out in        
    let g' = 
      VarSet.fold 
        (fun var new_graph -> 
          Printf.printf "Finding %s for g' block\n" (op2string var);
          print_graph g;
          OpMap.add 
            var
            (VarSet.union
              (if OpMap.mem var g then OpMap.find var g else empty_set)
              (VarSet.diff live_set (single var)))
            new_graph)
        live_set
        g
    in
    fold_insts live_set g' (List.rev b.gen_kill_sets.insts)
  in
  
  StringMap.fold (fun _ -> inter_build_block) cfg OpMap.empty
;;



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

