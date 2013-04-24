open Cfg_ast
open Cfg

type operandNode =
  | Normal of operand
  | Coalesced of OperandSet.t

module OperandNode = struct
  let compare = Pervasives.compare
  type t = operandNode
end

type iGraphEdge = 
  | MoveEdge of (operandNode * operandNode)
  | InterfereEdge of (operandNode * operandNode)

type iGraphEdgeType = E_Move | E_Interfere

module IGraphEdgeSet = 
  Set.Make(struct let compare = Pervasives.compare type t = iGraphEdge end)

type interfere_graph = IGraphEdgeSet.t

let nodes_equal n m = 
  let collapse = function Normal(o) -> single o | Coalesced(ol) -> ol in
    OperandSet.equal (collapse n) (collapse m)

let coalesce_nodes n = function
  | Normal(o) -> (
    match n with
    | Normal(p) -> Coalesced(OperandSet.add p (single o))
    | Coalesced(ol) -> Coalesced(OperandSet.add o ol)
  )

  | Coalesced(ol) -> (
    match n with
    | Normal(o) -> Coalesced(OperandSet.add o ol)
    | Coalesced(ol') -> Coalesced(OperandSet.union ol ol')
  )


let graph_add (l, r) edge_type g =
  if nodes_equal l r then
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

let rec opNode2str = function
  | Normal(o) -> op2string o
  | Coalesced(ol) -> String.concat "" (List.map op2string (OperandSet.elements ol))

let igedge2str = function
  | MoveEdge(o1, o2)      -> Printf.sprintf "%s <==> %s\n" (opNode2str o1) (opNode2str o2)
  | InterfereEdge(o1, o2) -> Printf.sprintf "%s <--> %s\n" (opNode2str o1) (opNode2str o2)

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
  
  let connect_set (set : OperandSet.t) =
    OperandSet.fold
      (fun var ->
        let other_nodes = OperandSet.diff set (single var) in
        OperandSet.fold
          (fun var' -> graph_add (Normal(var), Normal(var')) E_Interfere)
          other_nodes)
      set
  in

  let rec fold_insts (live_set : OperandSet.t) (g : interfere_graph) = function
    | { i = _; igen_set = gen; ikill_set = kill; move = _; }::tl ->
        let live' = OperandSet.diff live_set kill in
        (* Add edges from live_set nodes to gen set nodes *)
        let g' =
          OperandSet.fold
            (fun var -> 
              OperandSet.fold
                (fun var' -> 
                  graph_add (Normal(var), Normal(var')) E_Interfere)
                gen)
            live'
            g
        in
        let g'' = connect_set gen g' in
        (* Add gen set to live set *)
        let live'' = OperandSet.union gen live' in
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
            | { i = _; igen_set = _; ikill_set = _; move = Some((l, r)); } -> graph_add (Normal(l), Normal(r)) E_Move g
            | _ -> g)
          block.gen_kill_sets.insts)
      cfg
      no_moves

