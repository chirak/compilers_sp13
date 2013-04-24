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
type graphInfo = int NodeMap.t

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

let remove_move_edges (gi : graphInfo) (g : interfere_graph) =
    IGraphEdgeSet.fold
      (fun x a ->
        match x with
        | MoveEdge(l, r) -> NodeMap.remove l (NodeMap.remove r a)
        | _ -> a)
      g
      gi

let find_low_degree (gi : graphInfo) (k : int) : operandNode option =
  NodeMap.fold
    (fun operand degree -> function
      | Some(_) as s -> s
      | None -> if degree < k then Some(operand) else None)
    gi
    None

let remove_node (n : operandNode) =
  IGraphEdgeSet.filter 
    (function 
      | InterfereEdge(l, r) -> not (n = l || n = r) 
      | MoveEdge(l, r)      -> raise Impossible)

let find_neighbors x g =
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

let rec simplify (g : interfere_graph) (k : int) (stack : nodeStackMember list) : nodeStackMember list =
  if IGraphEdgeSet.is_empty g then 
    stack
  else
    let info = remove_move_edges (get_info g) g in
      match find_low_degree info k with
      | None -> coalesce g k stack
      | Some(operand) -> 
        simplify 
          (remove_node operand g)
          k 
          ((S_Normal(operand))::stack)

and coalesce (g : interfere_graph) (k : int) (stack : nodeStackMember list) : nodeStackMember list =
  let rec find_candidate = function
    | MoveEdge(l, r)::t ->
      (* Brigg's Conservative Coalescing Strategy *)
      let can_coalesce neighbors = 
        let gi = get_info g in
        let big_neighbors = 
          NodeSet.filter (fun o -> NodeMap.find o gi >= k) neighbors
        in
          NodeSet.cardinal big_neighbors < k
      in
      let left_edges = NodeSet.diff (find_neighbors l g) (singleton r) in
      let right_edges = NodeSet.diff (find_neighbors r g) (singleton l) in
      let combined = NodeSet.union left_edges right_edges in
        if can_coalesce combined then
          Some(((l, r), combined))
        else
          find_candidate t
    | _::t -> find_candidate t
    | [] -> None
  in
  let update_graph (l, r) c =
    IGraphEdgeSet.fold (
      fun x a ->
        let t = 
          match x with
          | InterfereEdge _ -> E_Interfere
          | MoveEdge _  -> E_Move
        in
        match x with InterfereEdge(o, p) | MoveEdge(o, p) ->
          let g_add b = graph_add b t a in
            if o = l then
              g_add (l, p)
            else if o = r then
              g_add (r, p)
            else if p = l then
              g_add (o, l)
            else if p = r then
              g_add (o, r)
            else
              IGraphEdgeSet.add x a)
    g
    IGraphEdgeSet.empty
  in
    match find_candidate (IGraphEdgeSet.elements g) with
    | Some(((l, r), c)) -> 
      simplify 
        (update_graph (l, r) c) 
        k 
        ((S_Normal(coalesce_nodes l r))::stack)
    | None -> freeze g k stack

and freeze g k stack =
  let have_frozen = ref false in
  let graph_info = get_info g in
  let g' = 
    IGraphEdgeSet.fold
      (fun edge a ->
        IGraphEdgeSet.add
          (match edge with
          | MoveEdge(l, r) when 
              not !have_frozen 
              && NodeMap.find l graph_info < k 
              && NodeMap.find r graph_info < k ->
            have_frozen := true;
            InterfereEdge(l, r)
          | _ -> edge)
          a)
      g
      IGraphEdgeSet.empty
  in
    if !have_frozen then
      simplify g' k stack
    else
      spill g k stack

and spill g k stack =
  let graph_info = get_info g in
  let (candidate, _) = 
    NodeMap.fold
      (fun node degree (candidate, candidate_degree) ->
        if degree > candidate_degree then
          (Some(node), degree)
        else
          (candidate, candidate_degree))
      graph_info
      (None, 0)
  in
    match candidate with
    | Some(node) -> simplify (remove_node node g) k (S_Spillable(node)::stack)
    | None       -> raise Impossible


module OperandMap = 
  Map.Make(struct let compare = Pervasives.compare type t = operand end)
type colorMap = Mips.reg OperandMap.t

module ColorSet = 
  Set.Make(struct let compare = Pervasives.compare type t = Mips.reg end)
type colorSet = ColorSet.t

type selectionResult =
  | Success of colorMap
  | Fail of operand


let rec select (g : interfere_graph) (stack : nodeStackMember list) (all_colors : colorSet) (a : colorMap) =
  match stack with
  | head::stack' -> (
    match head with S_Normal(node) | S_Spillable(node) ->
      let neighbors = find_neighbors node g in
      let unavailable_colors = 
        NodeSet.fold
          (fun node color_set ->
            let operands = node_operands node in
              OperandSet.fold
                (fun op color_set' ->
                  if OperandMap.mem op a then 
                    ColorSet.add (OperandMap.find op a) color_set'
                  else
                    color_set')
                operands
                color_set)
          neighbors
          ColorSet.empty
      in
      let available_colors = ColorSet.diff all_colors unavailable_colors in
        if not (ColorSet.is_empty available_colors) then
          let color = ColorSet.choose available_colors in
          let a' = 
            match node with
            | Normal(o)    -> OperandMap.add o color a 
            | Coalesced(s) -> OperandSet.fold (fun x a' -> OperandMap.add x color a') s a
          in
            select g stack' all_colors a'
        else (
          match head with
          | S_Spillable(Normal(operand)) -> Fail(operand)
          | _ -> raise Impossible
        )
    )
  | [] -> Success(a)


let assign_registers (f : func) (map : colorMap) : func = 
  let lookup x = Reg(OperandMap.find x map) in
  let assign_inst_registers = function
    | Move(l, r)            -> Move(lookup l, lookup r)
    | Arith(l, o1, ao, o2)  -> Arith(lookup l, lookup o1, ao, lookup o2)
    | Load(l, r, i)         -> Load(lookup l, lookup r, i)
    | Store(l, i, r)        -> Store(lookup l, i, lookup r)
    | Call(o)               -> Call(lookup o)
    | If(o1, c, o2, l1, l2) -> If(lookup o1, c, lookup o2, l1, l2)
    | x -> x
  in
  let rec assign_block_registers = function
    | inst::block -> (assign_inst_registers inst)::(assign_block_registers block)
    | []          -> []
  in
  let rec assign_func_registers = function
    | block::f' -> (assign_block_registers block)::(assign_func_registers f')
    | []        -> []
  in
    assign_func_registers f

let perform_spill (f : func) (spill : operand) =
  raise Implement_Me


(* Registers used for every function call:
 * Frame Pointer: $31
 * Return Address: $30
 * Callee Saved: $16 - $23 *)
let rec reg_alloc (f : func) : func = 
  let cfg = build_cfg f in
  let graph = build_interfere_graph cfg in
  let all_colors = raise Implement_Me in
  let stack = simplify graph (ColorSet.cardinal all_colors) [] in
  let pre_colored = OperandMap.empty in
    match select graph stack all_colors pre_colored with
    | Success(map) -> assign_registers f map
    | Fail(spill)  -> reg_alloc (perform_spill f spill)


(* Finally, translate the ouptut of reg_alloc to Mips instructions *)
let cfg_to_mips (f : func) : Mips.inst list = 
  raise Implement_Me

