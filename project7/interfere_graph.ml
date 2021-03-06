open Cfg_ast
open Cfg

(* Nodes for our interference graph can be either:
 *   -A single operand
 *   -A set of operands which occurs after nodes coalesce *)
type operandNode =
  | Normal of operand
  | Coalesced of OperandSet.t

(* Allows us to create Ocaml sets and maps for type operandNode *)
module OperandNode = struct
  let compare = Pervasives.compare
  type t = operandNode
end

(* General purpose set for OperandNodes *)
module NodeSet = Set.Make(OperandNode)
let singleton x = NodeSet.add x NodeSet.empty

(* Returns the Cfg_ast.operand for a given operandNode *)
let node_operands = function
  | Normal(o) -> single o
  | Coalesced(s) -> s

let rec opNode2str = function
  | Normal(o) -> op2string o
  | Coalesced(ol) -> String.concat "" (List.map op2string (OperandSet.elements ol))

type iGraphEdgeType = E_Move | E_Interfere
type iGraphEdge = 
  | MoveEdge of (operandNode * operandNode)
  | InterfereEdge of (operandNode * operandNode)

let igedge2str = function
  | MoveEdge(o1, o2)      ->
      Printf.sprintf "%s <==> %s\n" (opNode2str o1) (opNode2str o2)
  | InterfereEdge(o1, o2) ->
      Printf.sprintf "%s <--> %s\n" (opNode2str o1) (opNode2str o2)

module IGraphEdgeSet = 
  Set.Make(struct let compare = Pervasives.compare type t = iGraphEdge end)

(* The Interference Graph structure: 
 *   -Set of iGraphEdges
 *   -Set of nodes in graph
 *)
type interfere_graph = {
  nodes : NodeSet.t;
  edges : IGraphEdgeSet.t;
}
let empty_igraph = { nodes = NodeSet.empty; edges = IGraphEdgeSet.empty}
let igraph_equal g1 g2 : bool =
  NodeSet.equal g1.nodes g2.nodes && IGraphEdgeSet.equal g1.edges g2.edges

let igraph2string (i : interfere_graph) =
  String.concat 
    " "
    (List.map
      opNode2str
      (NodeSet.elements i.nodes))
  ^ "\n" ^
  String.concat 
    "" 
    (List.map 
      igedge2str
      (IGraphEdgeSet.elements i.edges))

let print_graph (i : interfere_graph) =
  print_string (igraph2string i)

(* Determines if two OperandNodes are the same.
   Note: Does NOT compare the edges of the node *)
let nodes_equal n m = 
  let collapse = function Normal(o) -> single o | Coalesced(ol) -> ol in
    OperandSet.equal (collapse n) (collapse m)

(* Coalesces two nodes *)
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

(* Adds an edge to the given interference graph. If the edge already exists,
 * we return the given graph*)
let graph_add (l, r) edge_type g =
  let edges =
    if nodes_equal l r then
      g.edges
    else (
      let c = Pervasives.compare l r in
      let e = if c < 0 then (l, r) else (r, l) in
      let edge = 
        match edge_type with
        | E_Move      -> MoveEdge e
        | E_Interfere -> InterfereEdge e
      in
        IGraphEdgeSet.add edge g.edges)
  in
  let nodes = NodeSet.add r (NodeSet.add l g.nodes) in
    { nodes = nodes; edges = edges}

(* given a function (i.e., list of basic blocks), constructs the
 * interference graph for that function. *)
let build_interfere_graph (cfg : cfg) : interfere_graph =
  
  (* Creates edges between all of the nodes in the given operand set and adds
   * it to the interference graph. *)
  let connect_set (set : OperandSet.t) =
    OperandSet.fold
      (fun var ->
        let other_nodes = OperandSet.diff set (single var) in
        OperandSet.fold
          (fun var' -> graph_add (Normal(var), Normal(var')) E_Interfere)
          other_nodes)
      set
  in

  let rec find_interferences (live_set : OperandSet.t) (g : interfere_graph) = function
    | { i = _; igen_set = gen; ikill_set = kill; move = _; }::tl ->
        (* Update live set for the current instruction *)
        let live' = OperandSet.diff live_set kill in
        (* Update the interference graph with with edges between the new live
         * set and the instrucion's gen set *)
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
        (* Update the interference graph with edges between all Operands in gen
         * set *)
        let g'' = connect_set gen g' in
        (* Update the live set to include the Operands in instructions gen
         * set *)
        let live'' = OperandSet.union gen live' in
          (* Recur for the rest of the instructions *)
          find_interferences live'' g'' tl
    | [] -> g
  in
  
  let inter_build_block (b : block_node) (g : interfere_graph) =
    let live_set = b.live_out in        
    (* All live_out nodes interfere, so add edges to interference graph before
     * traversing all instructions *)
    let g' = connect_set live_set g
    in
      (* Liveliness analysis works backwards so reverse the list *)
      find_interferences live_set g' (List.rev b.gen_kill_sets.insts)
  in
  (* Create an interference graph without move edges *)
  let no_moves = StringMap.fold (fun _ -> inter_build_block) cfg empty_igraph in

    (* Iterate through all of the cfg instructions and find move edges to put
     * into final interference graph. *)
    StringMap.fold
      (fun _ block -> 
        List.fold_right
          (fun inst g ->
            match inst with
            | { i = _; igen_set = _; ikill_set = _; move = Some((l, r)); } ->
                if is_var_or_reg l && is_var_or_reg r then
                  graph_add (Normal(l), Normal(r)) E_Move g
                else
                  g
            | _ -> g)
          block.gen_kill_sets.insts)
      cfg
      no_moves

