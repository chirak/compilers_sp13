open Cfg_ast
open Cfg
open Interfere_graph

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

module NodeMap = Map.Make(OperandNode)
type graph_info = int NodeMap.t

module NodeSet = Set.Make(OperandNode)
let singleton x = NodeSet.add x NodeSet.empty

type nodeStackMember =
  | S_Normal of operandNode
  | S_Spillable of operandNode

let get_info (g : interfere_graph) =
  let add2info x info =
    let amt = 
      if NodeMap.mem x info then
        ((NodeMap.find x info) + 1)
      else
        1
    in
      NodeMap.add x amt info
  in
    IGraphEdgeSet.fold
      (fun x a -> 
        match x with
        | InterfereEdge(l, r) -> 
          add2info r (add2info l a)
        | _ -> a)
      g
      NodeMap.empty

let remove_move_edges (gi : graph_info) (g : interfere_graph) =
    IGraphEdgeSet.fold
      (fun x a ->
        match x with
        | MoveEdge(l, r) ->
          NodeMap.remove l (NodeMap.remove r a)
        | _ -> a)
      g
      gi


let find_low_degree (gi : graph_info) (k : int) : operandNode option =
  NodeMap.fold
    (fun operand degree -> function
      | Some(_) as s -> s
      | None -> if degree < k then Some(operand) else None)
    gi
    None

let rec simplify (g : interfere_graph) (k : int) (stack : nodeStackMember list) : nodeStackMember list =
  let info = remove_move_edges (get_info g) g in
    match find_low_degree info k with
    | None -> coalesce g k stack
    | Some(operand) -> 
        simplify 
          (IGraphEdgeSet.filter 
            (function 
              | InterfereEdge(l, r) -> not (operand = l || operand = r) 
              | MoveEdge(l, r)      -> raise Impossible)
            g)
          k 
          ((S_Normal(operand))::stack)

and coalesce (g : interfere_graph) (k : int) (stack : nodeStackMember list) : nodeStackMember list =
  let rec find_candidate = function
    | MoveEdge(l, r)::t ->
      let find_edges x =
        IGraphEdgeSet.fold 
          (fun edge set -> 
            match edge with InterfereEdge(l', r') | MoveEdge(l', r') -> 
              if l' = x then
                NodeSet.add r' set 
              else if r' = x then
                NodeSet.add l' set
              else
                set)
            g
            NodeSet.empty
      in
      (* Brigg's Conservative Coalescing Strategy *)
      let can_coalesce neighbors = 
        let gi = get_info g in
        let big_neighbors = 
          NodeSet.filter (fun o -> NodeMap.find o gi >= k) neighbors
        in
          NodeSet.cardinal big_neighbors < k
      in
      let left_edges = NodeSet.diff (find_edges l) (singleton r) in
      let right_edges = NodeSet.diff (find_edges r) (singleton l) in
      let combined = NodeSet.union left_edges right_edges in
        if can_coalesce combined then
          Some(((l, r), combined))
        else
          find_candidate t
    | _::t -> find_candidate t
    | [] -> None
  in
  let update_graph (l, r) c g =
    raise Implement_Me
  in
    match find_candidate (IGraphEdgeSet.elements g) with
    | Some(((l, r), c)) -> simplify (update_graph (l, r) c g) k ((S_Normal(coalesce_nodes l r))::stack)
    | None -> freeze g k stack

and freeze g k stack =
  raise Implement_Me

(* Registers used for every function call:
 * Frame Pointer: $31
 * Return Address: $30
 * Callee Saved: $16 - $23 *)
let reg_alloc (f : func) : func = 
  raise Implement_Me

(* Finally, translate the ouptut of reg_alloc to Mips instructions *)
let cfg_to_mips (f : func ) : Mips.inst list = 
  raise Implement_Me

