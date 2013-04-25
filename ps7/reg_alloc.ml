open Cfg_ast
open Cfg
open Interfere_graph

module NodeMap = Map.Make(OperandNode)
(* Maps an OperandNode to the degree (# of edges) of the OperandNode *)
type graphInfo = int NodeMap.t

let graphInfo2string (gi : graphInfo) : string =
  NodeMap.fold (fun operand degree str ->
                  let s = Printf.sprintf "%s -> %d\n" (opNode2str operand) degree in
                    str^s) gi ""

type nodeStackMember =
  | S_Normal of operandNode
  | S_Spillable of operandNode

(* Generates a graphInfo from the given interference graph. Only interference
 * edges are counted, but move related nodes are still included! *)
let get_info (g : interfere_graph) : graphInfo =
  let add2info x info =
    let amt = 
      if NodeMap.mem x info then ((NodeMap.find x info) + 1) else 1
    in
      NodeMap.add x amt info
  in
  (* Degree map based on edges *)
  let edges_map = 
    IGraphEdgeSet.fold
      (fun x a -> 
        match x with
        | InterfereEdge(l, r) -> 
          add2info r (add2info l a)
        | _ -> a)
      g.edges
      NodeMap.empty
  in
    (* Find all nodes of zero degree *)
    NodeSet.fold
      (fun x a ->
        if not (NodeMap.mem x a) then
          NodeMap.add x 0 a
        else
          a)
      g.nodes
      edges_map

(* Removes move related nodes from the given graphInfo. Interference graph is
 * required to determine which nodes are move related *)
let remove_move_nodes (gi : graphInfo) (g : interfere_graph) : graphInfo =
    IGraphEdgeSet.fold
      (fun x a ->
        match x with
        | MoveEdge(l, r) -> NodeMap.remove l (NodeMap.remove r a)
        | _ -> a)
      g.edges
      gi

(* Finds a node with a degree less than k in the given graphInfo *)
let find_low_degree (gi : graphInfo) (k : int) : operandNode option =
  let result =
    NodeMap.fold
      (fun operand degree -> function
        | Some((o, d)) as x -> if degree < d then Some((operand, degree)) else x
        | None              -> if degree < k then Some((operand, degree)) else None)
      gi
      None
  in
    match result with
    | Some((o, d)) -> Some(o)
    | None         -> None

(* Removes all of the edges with the given node in the given interference graph.
 *)
let remove_node (n : operandNode) : (IGraphEdgeSet.t -> IGraphEdgeSet.t) =
  IGraphEdgeSet.filter 
    (function InterfereEdge(l, r) | MoveEdge(l, r) -> not (n = l || n = r))
            

(* Returns a set of neighbors of the given node *)
let find_neighbors (x : operandNode) (g : interfere_graph) : NodeSet.t =
  IGraphEdgeSet.fold 
    (fun edge set -> 
       (* TODO should we include MoveEdges here?? *)
      match edge with InterfereEdge(l', r') | MoveEdge(l', r') -> 
        if l' = x then
          NodeSet.add r' set 
        else if r' = x then
          NodeSet.add l' set
        else
          set)
      g.edges
      NodeSet.empty

let rec simplify (g : interfere_graph) (k : int) (stack : nodeStackMember list) : nodeStackMember list =
  print_string "Simplifying...\n";
  if NodeSet.is_empty g.nodes then 
    stack
  else
    let info = remove_move_nodes (get_info g) g in
      print_string (graphInfo2string info);
      match find_low_degree info k with
      | None -> coalesce g k stack
      | Some(node) -> 
          print_string ("Removing node: " ^ (opNode2str node) ^ "\n");
          simplify 
            { nodes=NodeSet.remove node g.nodes; edges=(remove_node node g.edges); }
            k 
            ((S_Normal(node))::stack)

and coalesce (g : interfere_graph) (k : int) (stack : nodeStackMember list) : nodeStackMember list =
  print_string "Coallescing...\n";
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
        let left_edges  = NodeSet.diff (find_neighbors l g) (singleton r) in
        let right_edges = NodeSet.diff (find_neighbors r g) (singleton l) in
        let combined    = NodeSet.union left_edges right_edges in
          if not (IGraphEdgeSet.mem (InterfereEdge(l, r)) g.edges) && can_coalesce combined then
            Some(((l, r), combined))
          else
            find_candidate t
    | _::t -> find_candidate t
    | [] -> None
  in
  let update_graph (l, r) coalesced neighbors =
    let (filtered, neighborMap) = 
      IGraphEdgeSet.fold (
        fun edge (filtered, neighborMap) ->
          let t =
            match edge with
            | InterfereEdge(l', r') -> E_Interfere
            | MoveEdge(l', r')      -> E_Move
          in
            match edge with InterfereEdge(o, p) | MoveEdge(o, p) ->
              if o = l || o = r then
                if NodeMap.mem p neighborMap then
                  (filtered, NodeMap.add p E_Interfere neighborMap)
                else
                  (filtered, NodeMap.add p t neighborMap)
              else if p = l || p = r then
                if NodeMap.mem o neighborMap then
                  (filtered, NodeMap.add o E_Interfere neighborMap)
                else
                  (filtered, NodeMap.add o t neighborMap)
              else
                (IGraphEdgeSet.add edge filtered, neighborMap))
        g.edges
        (IGraphEdgeSet.empty, NodeMap.empty)
    in
      NodeSet.fold (
        fun neighbor ->
          let t = NodeMap.find neighbor neighborMap in
            graph_add (coalesced, neighbor) t)
        neighbors
        { 
          nodes=NodeSet.remove l (NodeSet.remove r g.nodes); 
          edges=filtered; 
        }
  in
    match find_candidate (IGraphEdgeSet.elements g.edges) with
    | Some(((l, r), neighbors)) -> 
        print_string ("Coalescing nodes: " ^ (opNode2str l) ^ " and " ^ (opNode2str r) ^ "\n");
        let coalesced = coalesce_nodes l r in
          simplify 
            (update_graph (l, r) coalesced neighbors) 
            k 
            stack
    | None -> resolve_constraints g k stack

and resolve_constraints g k stack =
  print_string "Resolving Constraints...\n";
  let g' = 
    IGraphEdgeSet.filter
      (function
        | MoveEdge(l, r) 
          when IGraphEdgeSet.mem (InterfereEdge(l, r)) g.edges ->
            print_string ("Resolved contraint between nodes " ^ (opNode2str l) ^ " and " ^ (opNode2str r) ^ "\n");
            false
        | _ -> true)
      g.edges
  in
    if IGraphEdgeSet.equal g.edges g' then
      freeze g k stack
    else
      simplify { nodes=g.nodes; edges=g'; } k stack

and freeze g k stack =
  print_string "Freezing...\n";
  let have_frozen = ref false in
  let graph_info = get_info g in
  let g' = 
    IGraphEdgeSet.fold
      (fun edge a ->
        IGraphEdgeSet.add
          (match edge with
          | MoveEdge(l, r) 
            when not !have_frozen 
              && NodeMap.find l graph_info < k 
              && NodeMap.find r graph_info < k ->
                print_string ("Freezing move between nodes " ^ (opNode2str l) ^ " and " ^ (opNode2str r) ^ "\n");
                have_frozen := true;
                InterfereEdge(l, r)
          | _ -> edge)
          a)
      g.edges
      IGraphEdgeSet.empty
  in
    if !have_frozen then
      simplify { nodes=g.nodes; edges=g'; } k stack
    else
      spill g k stack

and spill g k stack =
  print_string "Marking Potential Spills...\n";
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
    | Some(node) -> 
        print_string ("Marking node " ^ (opNode2str node) ^ " as a potential spill candidate.\n");
        simplify 
          { nodes=NodeSet.remove node g.nodes; edges=remove_node node g.edges; } 
          k 
          (S_Spillable(node)::stack)
    | None -> raise Impossible


module OperandMap = 
  Map.Make(struct let compare = Pervasives.compare type t = operand end)
type colorMap = Mips.reg OperandMap.t

let colorMap2string (gi : colorMap) : string =
  OperandMap.fold 
    (fun operand reg str ->
      let s = Printf.sprintf "%s -> %s\n" (op2string operand) (Mips.reg2string reg) in
        str^s) 
    gi 
    ""

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
        let operands = node_operands node in
        let unavailable_colors = 
          OperandSet.fold (
            fun operand color_set ->
              let neighbors = find_neighbors (Normal(operand)) g in
                NodeSet.fold
                  (fun node' color_set' ->
                    let operands' = node_operands node' in
                      OperandSet.fold
                        (fun op color_set'' ->
                          if OperandMap.mem op a then ( 
                            let c = (OperandMap.find op a) in
                              print_string ("Neighbor " ^ (op2string op) ^ " has color " ^ (Mips.reg2string c) ^ "\n");
                              ColorSet.add c color_set''
                          ) else (
                            print_string ("Neighbor " ^ (op2string op) ^ " has no color.\n");
                            color_set''))
                        operands'
                        color_set')
                  neighbors
                  color_set)
            operands
            ColorSet.empty
        in
        print_string ("Unavailable Registers for " ^ (opNode2str node) ^ ": " ^ String.concat ", " (List.map Mips.reg2string (ColorSet.elements unavailable_colors)) ^ "\n");
        let available_colors = ColorSet.diff all_colors unavailable_colors in
          if not (ColorSet.is_empty available_colors) then
            let color = ColorSet.choose available_colors in
            let a' = 
              match node with
              | Normal(o)    -> OperandMap.add o color a 
              | Coalesced(s) -> OperandSet.fold (fun x a'' -> OperandMap.add x color a'') s a
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
  let lookup x =
    match x with
    | Var(v) -> 
        if OperandMap.mem x map then
          Reg(OperandMap.find x map)
        else
          Reg(Mips.R24)
    | _ -> x
  in
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

let perform_spill (f : func) (spill : operand) : func =
  raise Implement_Me
  (* let spilled = ref false in
  let offset = ref 0 in
  let rec spill_inst a x =
    match x with
    | Move(l, r) when l = spill ->
        if not !spilled then (
          spilled := true;
        )
        (Store(fp, !offset-4, spill))::x::a
    
    | Arith(l, r1, o, r2) when l = spill ->
        if not !spilled then (
          spilled := true;
        )
        (Store(fp, !offset-4, spill))::x::a

    | Load(l, r, i) ->
    
    | Store(l, i, r) when l = fp ->
        offset := i+4;
        (Store(l, !offset, r))::x::a

    | _ -> x::a
  in
  let rec spill_block a = function
    | inst::b' -> spill_block (spill_inst a inst) b'
    | []       -> List.reverse a
  in
  let rec spill_func = function
    | block::f' -> (spill_block [] block)::(spill_func f')
    | []        -> []
  in *)


(* Registers used for every function call:
 * Frame Pointer: $31
 * Return Address: $30
 * Callee Saved: $16 - $23 *)
let rec reg_alloc (f : func) : func = 
  let cfg = build_cfg f in
  let graph = build_interfere_graph cfg in
  print_graph graph;
  let all_colors = 
    List.fold_right
      ColorSet.add
      [
        Mips.R8;  Mips.R9;  Mips.R10;
        Mips.R11; Mips.R12; Mips.R13;
        Mips.R14; Mips.R15; Mips.R16;
        Mips.R17; Mips.R18; Mips.R19;
        Mips.R20; Mips.R21; Mips.R22;
        Mips.R23;
      ]
      ColorSet.empty
  in
  let stack = simplify graph (ColorSet.cardinal all_colors) [] in
  print_string ("Stack:\n" ^ String.concat "\n" (List.map (function S_Spillable(o) -> "sp("^(opNode2str o)^")" | S_Normal(o) -> (opNode2str o)) stack) ^ "\n\n");
  let pre_colored = 
    NodeSet.fold
      (fun node color_map ->
        match node with
          | Normal(Reg(r) as x) -> OperandMap.add x r color_map
          | _ -> color_map)
      graph.nodes
      OperandMap.empty
  in
    match select graph stack all_colors pre_colored with
    | Success(map) -> print_string (colorMap2string map); assign_registers f map
    | Fail(spill)  -> reg_alloc (perform_spill f spill)


(* Finally, translate the ouptut of reg_alloc to Mips instructions *)
let cfg_to_mips (f : func) : Mips.inst list = 
  let op2mipsop = function
    | Int(i) -> Mips.Immed(Word32.fromInt i)
    | Reg(r) -> Mips.Reg(r)
    | Var _  -> error "Found var in CFG instruction"
    | _ -> raise Impossible
  in
  let instr2mips (a : Mips.inst list) = function
    | Label(l) -> Mips.Label(l)::a
    | Move(Reg(l), r) -> Mips.Or(l, Mips.R0, (op2mipsop r))::a
    | Arith(Reg(l), r1, ao, r2) -> (
        match ao with
        | Plus ->
            let a' = Mips.Or(l, Mips.R0, op2mipsop r1)::a in
              Mips.Add(l, l, op2mipsop r1)::a'
        | Minus ->
            let a' = Mips.Or(l, Mips.R0, op2mipsop r1)::a in (
              match r2 with
              | Int(i) -> Mips.Add(l, l, Mips.Immed(Word32.fromInt (-i)))
              | Reg(r) -> Mips.Sub(l, l, r)
              | _ -> raise Impossible
            )::a'
        | Times ->
            let a' = Mips.Or(l, Mips.R0, op2mipsop r1)::a in (
              match r2 with
              | Int(i) -> 
                  let a'' = Mips.Or(Mips.R25, Mips.R0, Mips.Immed(Word32.fromInt i))::a' in
                    Mips.Mul(l, l, Mips.R25)::a''
              | Reg(r) -> Mips.Mul(l, l, r)::a'
              | _ -> raise Impossible
            )
        | Div ->
            let a' = Mips.Or(l, Mips.R0, op2mipsop r1)::a in (
              match r2 with
              | Int(i) -> 
                  let a'' = Mips.Or(Mips.R25, Mips.R0, Mips.Immed(Word32.fromInt i))::a' in
                    Mips.Div(l, l, Mips.R25)::a''
              | Reg(r) -> Mips.Div(l, l, r)::a'
              | _ -> raise Impossible
            )
      )
    | Load(Reg(l), Reg(r), i) -> Mips.Lw(l, r, Word32.fromInt i)::a
    | Store(Reg(l), i, Reg(r)) -> Mips.Sw(r, l, Word32.fromInt i)::a
    | Call(Lab(l)) -> Mips.Jal(l)::a
    | Jump(l) -> Mips.J(l)::a
    | If(o1, c, o2, l1, l2) -> (
        match c with
        | Eq  ->
            let a = Mips.J(l2)::a in
            let a = Mips.Beq(Mips.R24, Mips.R25, l1)::a in
            let a = Mips.Or(Mips.R24, Mips.R0, op2mipsop o1)::a in
              Mips.Or(Mips.R25, Mips.R0, op2mipsop o2)::a
        | Neq ->
            let a = Mips.J(l2)::a in
            let a = Mips.Bne(Mips.R24, Mips.R25, l1)::a in
            let a = Mips.Or(Mips.R24, Mips.R0, op2mipsop o1)::a in
              Mips.Or(Mips.R25, Mips.R0, op2mipsop o2)::a
        | Lt  ->
            let a = Mips.J(l2)::a in
            let a = Mips.Blt(Mips.R24, Mips.R25, l1)::a in
            let a = Mips.Or(Mips.R24, Mips.R0, op2mipsop o1)::a in
              Mips.Or(Mips.R25, Mips.R0, op2mipsop o2)::a
        | Lte ->
            let a = Mips.J(l2)::a in
            let a = Mips.Ble(Mips.R24, Mips.R25, l1)::a in
            let a = Mips.Or(Mips.R24, Mips.R0, op2mipsop o1)::a in
              Mips.Or(Mips.R25, Mips.R0, op2mipsop o2)::a
        | Gt  ->
            let a = Mips.J(l2)::a in
            let a = Mips.Bgt(Mips.R24, Mips.R25, l1)::a in
            let a = Mips.Or(Mips.R24, Mips.R0, op2mipsop o1)::a in
              Mips.Or(Mips.R25, Mips.R0, op2mipsop o2)::a
        | Gte ->
            let a = Mips.J(l2)::a in
            let a = Mips.Bge(Mips.R24, Mips.R25, l1)::a in
            let a = Mips.Or(Mips.R24, Mips.R0, op2mipsop o1)::a in
              Mips.Or(Mips.R25, Mips.R0, op2mipsop o2)::a
      )
    | Return -> Mips.Jr(Mips.R31)::a
    | _ -> raise Impossible
  in
  let rec block2mips (a : Mips.inst list) = function
    | inst::b' -> block2mips (instr2mips a inst) b'
    | [] -> a
  in
  let rec func2mips (a : Mips.inst list) = function
    | block::f' -> func2mips (block2mips a block) f'
    | []        -> List.rev a
  in
    List.filter 
      (function
        | Mips.Or(l, Mips.R0, Mips.Reg(r)) when l = r -> false
        | _ -> true)
      (func2mips [] f)

