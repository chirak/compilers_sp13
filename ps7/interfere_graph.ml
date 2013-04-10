open Cfg_ast
open Cfg

type iGraphEdge = (operand * operand)

let compare_edge (o1a, o1b) (o2a, o2b) =
  if o1a = o2a || o1a = o2b && o1b = o2a || o1b = o2b then 0 else Pervasives.compare (o1a, o1b) (o2a, o2b)

module IGraphEdgeSet = 
  Set.Make(struct let compare = compare_edge type t = iGraphEdge end)

type interfere_graph = IGraphEdgeSet.t

let print_graph (i : interfere_graph) =
  IGraphEdgeSet.iter 
    (fun (o1, o2) -> Printf.printf "%s <--> %s\n" (op2string o1) (op2string o2))
    i

(* given a function (i.e., list of basic blocks), construct the
 * interference graph for that function.  This will require that
 * you build a dataflow analysis for calculating what set of variables
 * are live-in and live-out for each program point. *)
let build_interfere_graph (cfg : cfg) : interfere_graph =
  
  let rec fold_insts (live_set : VarSet.t) (g : interfere_graph) = function
    | { i = _; igen_set = gen; ikill_set = kill; }::tl ->
        let live' = VarSet.diff live_set kill in
        (* Add edges from live_set nodes to gen set nodes *)
        let g' =
          VarSet.fold
            (fun var -> 
              VarSet.fold
                (fun var' -> IGraphEdgeSet.add (var, var'))
                gen)
            live_set
            g
        in
        (* Add gen set to live set *)
        let live'' = VarSet.union gen live' in
          (* Recur *)
          fold_insts live'' g' tl
    | [] -> g
  in
  
  let inter_build_block (b : block_node) (g : interfere_graph) =
    let live_set = b.live_out in        
    let g' = 
      VarSet.fold 
        (fun var -> 
          let other_nodes = VarSet.diff live_set (single var) in
            VarSet.fold
              (fun var' -> IGraphEdgeSet.add (var, var'))
              other_nodes)
        live_set
        g
    in
      fold_insts live_set g' (List.rev b.gen_kill_sets.insts)
  in
    StringMap.fold (fun _ -> inter_build_block) cfg IGraphEdgeSet.empty

