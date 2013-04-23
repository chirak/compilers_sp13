open Cfg_ast
open Cfg

type iGraphEdge = 
  | MoveEdge of (operand * operand)
  | InterfereEdge of (operand * operand)

type iGraphEdgeType = E_Move | E_Interfere

module IGraphEdgeSet = 
  Set.Make(struct let compare = Pervasives.compare type t = iGraphEdge end)

type interfere_graph = IGraphEdgeSet.t

let graph_add (l, r) edge_type g =
  if l = r then
    g
  else (
    let c = Pervasives.compare l r in
    let e = if c < 0 then (l, r) else (r, l) in
    let edge = 
      match edge_type with
      | E_Move      -> MoveEdge e
      | E_Interfere -> InterfereEdge e
    in
      IGraphEdgeSet.add edge g
  )

let igedge2str = function
  | MoveEdge(o1, o2)      -> Printf.sprintf "%s <==> %s\n" (op2string o1) (op2string o2)
  | InterfereEdge(o1, o2) -> Printf.sprintf "%s <--> %s\n" (op2string o1) (op2string o2)

let igraph2string (i : interfere_graph) =
  String.concat "" 
    (List.map 
      (fun edge -> igedge2str edge)
      (IGraphEdgeSet.elements i))

let print_graph (i : interfere_graph) =
  print_string (igraph2string i)

(* given a function (i.e., list of basic blocks), construct the
 * interference graph for that function.  This will require that
 * you build a dataflow analysis for calculating what set of variables
 * are live-in and live-out for each program point. *)
let build_interfere_graph (cfg : cfg) : interfere_graph =
  
  let connect_set (set : VarSet.t) =
    VarSet.fold
      (fun var ->
        let other_nodes = VarSet.diff set (single var) in
        VarSet.fold
          (fun var' -> graph_add (var, var') E_Interfere)
          other_nodes)
      set
  in

  let rec fold_insts (live_set : VarSet.t) (g : interfere_graph) = function
    | { i = _; igen_set = gen; ikill_set = kill; move = _; }::tl ->
        let live' = VarSet.diff live_set kill in
        (* Add edges from live_set nodes to gen set nodes *)
        let g' =
          VarSet.fold
            (fun var -> 
              VarSet.fold
                (fun var' -> 
                  graph_add (var, var') E_Interfere)
                gen)
            live'
            g
        in
        let g'' = connect_set gen g' in
        (* Add gen set to live set *)
        let live'' = VarSet.union gen live' in
          (* Recur *)
          fold_insts live'' g'' tl
    | [] -> g
  in
  
  let inter_build_block (b : block_node) (g : interfere_graph) =
    let live_set = b.live_out in        
    let g' = connect_set live_set g
    in
      fold_insts live_set g' (List.rev b.gen_kill_sets.insts)
  in
  let no_moves = StringMap.fold (fun _ -> inter_build_block) cfg IGraphEdgeSet.empty in
    StringMap.fold
      (fun _ block -> 
        List.fold_right
          (fun inst g ->
            match inst with
            | { i = _; igen_set = _; ikill_set = _; move = Some(move); } -> graph_add move E_Move g
            | _ -> g)
          block.gen_kill_sets.insts)
      cfg
      no_moves

