open Cfg_ast
open Cfg

module OpMap = 
  Map.Make(struct let compare = Pervasives.compare type t = operand end)
type interfere_graph = VarSet.t OpMap.t;;
let print_graph (i : interfere_graph) =
  OpMap.iter (fun k v ->
                    let nodes = vs2string v in
                      Printf.printf "%s -> %s\n" (op2string k) nodes) i

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
            (fun x a -> 
               let connectors = OpMap.find x a in
               let connectors' = VarSet.union gen connectors in
                 OpMap.add x connectors' a)
            live_set
            g
        in
        (* Add edges from gen set nodes to live_set nodes *)
        let g'' = 
          VarSet.fold 
            (fun x a -> 
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

