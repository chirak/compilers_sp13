open Cfg_ast
open Cfg
open Interfere_graph

module NodeMap = Map.Make(OperandNode)
(* Maps an OperandNode to the degree (# of edges) of the OperandNode *)
type graphInfo = int NodeMap.t


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

(* Serializes a graph info structure *)
let graphInfo2string (gi : graphInfo) : string =
  NodeMap.fold (fun operand degree str ->
                  let s = Printf.sprintf "%s -> %d\n" (opNode2str operand) degree in
                    str^s) gi ""

type nodeStackMember =
  | S_Normal of operandNode
  | S_Spillable of operandNode

(* Generates a graphInfo from the given interference graph. Only interference
 * edges are counted, but move related nodes are still included! *)
let get_info (g : interfere_graph) (all_colors : colorSet) : graphInfo =
  let add2info x info =
    let amt = 
      if NodeMap.mem x info then ((NodeMap.find x info) + 1) else 1
    in
      NodeMap.add x amt info
  in
  (* Determines if node is pre-colored or not with one of our available colors *)
  let is_valid = function 
    | Normal(Reg(r)) -> ColorSet.mem r all_colors
    | Coalesced(operands) ->
        OperandSet.for_all (
          function 
            | Reg(r) -> ColorSet.mem r all_colors
            | _ -> true)
          operands
    | _ -> true
  in
  (* Degree map based on edges *)
  let edges_map = 
    IGraphEdgeSet.fold
      (fun x a -> 
        match x with
        | InterfereEdge(l, r) when is_valid l && is_valid r -> 
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

(* Determines if a node is pre-colored *)
let is_precolored = function
  | Normal(Reg(_)) -> true
  | Coalesced(o) when OperandSet.exists (function Reg(_) -> true | _ -> false) o -> true
  | _ -> false

(* Removes all pre-colored bindings from a graph_info structure *)
let remove_pre_colored gi =
  NodeMap.fold (
    fun node degree new_map ->
      if is_precolored node then
        new_map
      else
        NodeMap.add node degree new_map)
    gi
    NodeMap.empty


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
      (fun operand degree a ->
        if is_precolored operand then
          a
        else
          match a with
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


(* -----------THE ALGORITHM-------------- *)

let rec simplify (g : interfere_graph) (all_colors : colorSet) (stack : nodeStackMember list) : nodeStackMember list =
  (* If there's nothing left in the graph, return the stack *)
  if NodeSet.is_empty g.nodes then 
    stack
  else
    let info = remove_move_nodes (get_info g all_colors) g in
      (* If there are only pre-colored nodes left, we're done *)
      if List.for_all is_precolored (NodeSet.elements g.nodes) then
        (List.map (fun x -> S_Normal(x)) (NodeSet.elements g.nodes)) @ stack
      else
        (* Try to select a node *)
        match find_low_degree info (ColorSet.cardinal all_colors) with
        | None -> coalesce g all_colors stack
        | Some(node) -> 
            simplify 
              { nodes=NodeSet.remove node g.nodes; edges=(remove_node node g.edges); }
              all_colors 
              ((S_Normal(node))::stack)

and coalesce (g : interfere_graph) (all_colors : colorSet) (stack : nodeStackMember list) : nodeStackMember list =
  let k = ColorSet.cardinal all_colors in
  (* Finds a potential candidate for coalescing *)
  let rec find_candidate = function
    | MoveEdge(l, r)::t ->
        (* Brigg's Conservative Coalescing Strategy *)
        let can_coalesce neighbors = 
          let gi = get_info g all_colors in
          let big_neighbors = 
            NodeSet.filter (fun o -> is_precolored o || NodeMap.find o gi >= k) neighbors
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
  (* Restructures graph after a node coalesce *)
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
          nodes=NodeSet.add coalesced (NodeSet.remove l (NodeSet.remove r g.nodes)); 
          edges=filtered; 
        }
  in
    (* Try to find an edge to coalesce *)
    match find_candidate (IGraphEdgeSet.elements g.edges) with
    | Some(((l, r), neighbors)) -> 
        let coalesced = coalesce_nodes l r in
          simplify 
            (update_graph (l, r) coalesced neighbors) 
            all_colors 
            stack
    | None -> resolve_constraints g all_colors stack

and resolve_constraints g all_colors stack =
  (* Try to remove MoveEdges that have an equivalent InterfereEdge *)
  let g' = 
    IGraphEdgeSet.filter
      (function
        | MoveEdge(l, r) 
          when IGraphEdgeSet.mem (InterfereEdge(l, r)) g.edges ->
            false
        | _ -> true)
      g.edges
  in
    if IGraphEdgeSet.equal g.edges g' then
      freeze g all_colors stack
    else
      simplify { nodes=g.nodes; edges=g'; } all_colors stack

and freeze g all_colors stack =
  let have_frozen = ref false in
  let graph_info = get_info g all_colors in
  let k = ColorSet.cardinal all_colors in
  (* Find an edge that can be frozen *)
  let g' = 
    IGraphEdgeSet.fold
      (fun edge a ->
        IGraphEdgeSet.add
          (match edge with
          | MoveEdge(l, r) 
            when not !have_frozen && ((not (is_precolored l) && NodeMap.find l graph_info < k) || (not (is_precolored r) && NodeMap.find r graph_info < k)) ->
                have_frozen := true;
                InterfereEdge(l, r)
          | _ -> edge)
          a)
      g.edges
      IGraphEdgeSet.empty
  in
    if !have_frozen then
      simplify { nodes=g.nodes; edges=g'; } all_colors stack
    else
      spill g all_colors stack

and spill g all_colors stack =
  let graph_info = get_info g all_colors in
  (* Find a spill candidate *)
  let (candidate, _) = 
    NodeMap.fold
      (fun node degree (candidate, candidate_degree) ->
        if not (is_precolored node) && degree > candidate_degree then
          (Some(node), degree)
        else
          (candidate, candidate_degree))
      graph_info
      (None, 0)
  in
    match candidate with
    | Some(node) -> 
        simplify 
          { nodes=NodeSet.remove node g.nodes; edges=remove_node node g.edges; } 
          all_colors 
          (S_Spillable(node)::stack)
    | None -> raise Impossible


(* Output of the selection algorithm *)
type selectionResult =
  | Success of colorMap
  | Fail of operand


let rec select (g : interfere_graph) (stack : nodeStackMember list) (all_colors : colorSet) (a : colorMap) =
  match stack with
  | head::stack' -> (
      match head with S_Normal(node) | S_Spillable(node) ->
        let operands = node_operands node in
        (* find all available colors for this stack element *)
        let available_colors =
          (* Determine if this element has an inherent color *)
          let pre_colored = 
            OperandSet.fold 
              (fun o -> function 
                | Some(c) -> Some(c) 
                | None -> 
                  if (OperandMap.mem o a) then 
                    Some(OperandMap.find o a) 
                  else 
                    None) 
              operands 
              None
          in
            match pre_colored with
            | Some(color) -> ColorSet.add color ColorSet.empty
            | None -> 
                (* Determine colors of all neighbors *)
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
                                      (* print_string ("Neighbor " ^ (op2string op) ^ " has color " ^ (Mips.reg2string c) ^ "\n"); *)
                                      ColorSet.add c color_set''
                                  ) else (
                                    (* print_string ("Neighbor " ^ (op2string op) ^ " has no color.\n"); *)
                                    color_set''))
                                operands'
                                color_set')
                          neighbors
                          color_set)
                    operands
                    ColorSet.empty
                in
                  (* Get all available colors by taking a set difference *)
                  ColorSet.diff all_colors unavailable_colors 
        in
          if not (ColorSet.is_empty available_colors) then
            let color = ColorSet.choose available_colors in
            let a' = 
              match node with
              | Normal(o)    -> OperandMap.add o color a 
              | Coalesced(s) -> OperandSet.fold (fun o a'' -> OperandMap.add o color a'') s a
            in
              select g stack' all_colors a'
          else
            match head with
            | S_Spillable(Normal(operand)) -> Fail(operand)
            | _ -> raise Impossible
      )
  | [] -> Success(a)

(* Replaces all temps with registers *)
let assign_registers (f : func) (map : colorMap) : func = 
  let lookup x =
    match x with
    | Var(v) -> 
        if OperandMap.mem x map then
          Reg(OperandMap.find x map)
        else (
          print_string ("Could not find color for " ^ (op2string x) ^"\n");
          Reg(Mips.R24) (* HACK *)
        )
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

(* Rewrites a func so that the given operand is spilled *)
let perform_spill (f : func) (spill : operand) (spill_amt : int) : func =
  (* let loaded = ref false in *)
  raise Implement_Me;
  let spill_inst a x =
    match x with
    | Move(l, _) | Arith(l, _, _, _) | Load(l, _, _)
      when l = spill ->
        (* loaded := false; *)
        Store(sp, spill_amt*4, l)::x::a

    | Move(l, r) when r = spill ->
        let new_var = Var("_"^(op2string r)^"_s") in
          (* if !loaded then
            Move(l, new_var)::a
          else ( 
          loaded := true;  *)
            Move(l, new_var)
            ::Load(new_var, sp, spill_amt*4)
            ::a
          (* ) *)
    | Arith(l, r, o, r2) when r = spill ->
        let new_var = Var("_"^(op2string r)^"_s") in
          (* if !loaded then
            Arith(l, new_var, o, r2)::a
          else ( 
            loaded := true; *)
            Arith(l, new_var, o, r2)
            ::Load(new_var, sp, spill_amt*4)
            ::a
          (* ) *)
    | Arith(l, r1, o, r) when r = spill ->
        let new_var = Var("_"^(op2string r)^"_s") in
          (* if !loaded then
            Arith(l, r1, o, new_var)::a
          else (
            loaded := true; *)
            Arith(l, r1, o, new_var)
            ::Load(new_var, sp, spill_amt*4)
            ::a
         (*  ) *)
    | Load(l, r, i) when r = spill ->
        let new_var = Var("_"^(op2string r)^"_s") in
          (* if !loaded then
            Load(l, new_var, i)::a
          else (
            loaded := true;
     *)        Load(l, new_var, i)
            ::Load(new_var, sp, spill_amt*4)
            ::a
         (*  ) *)
    | Store(r, i, l) when r = spill ->
        let new_var = Var("_"^(op2string r)^"_s") in
          (* if !loaded then
            Store(new_var, i, l)::a
          else (
            loaded := true;
    *)         Store(new_var, i, l)
            ::Load(new_var, sp, spill_amt*4)
            ::a
        (*   ) *)
    | Store(l, i, r) when r = spill ->
        let new_var = Var("_"^(op2string r)^"_s") in
          (* if !loaded then
            Store(l, i, new_var)::a
          else (
            loaded := true;
    *)         Store(l, i, new_var)
            ::Load(new_var, sp, spill_amt*4)
            ::a
        (*   ) *)
    | Call(r) when r = spill ->
        let new_var = Var("_"^(op2string r)^"_s") in
          (* if !loaded then
            Call(new_var)::a
          else (
            loaded := true;
           *)  Call(new_var)
            ::Load(new_var, sp, spill_amt*4)
            ::a
        (*   ) *)
    | If(r, x, b, c, d) when r = spill ->
        let new_var = Var("_"^(op2string r)^"_s") in
          (* if !loaded then
            If(new_var, x, b, c, d)::a
          else (
            loaded := true;
 *)            If(new_var, x, b, c, d)
            ::Load(new_var, sp, spill_amt*4)
            ::a
        (*   ) *)
    | If(x, b, r, c, d) when r = spill ->
        let new_var = Var("_"^(op2string r)^"_s") in
          (* if !loaded then
            If(x, b, new_var, c, d)::a
          else (
            loaded := true;
 *)            If(x, b, new_var, c, d)
            ::Load(new_var, sp, spill_amt*4)
            ::a
        (*   ) *)
    | _ -> x::a
  in
  let rec spill_block a = function
    | inst::b' -> spill_block (spill_inst a inst) b'
    | []       -> List.rev a
  in
  let rec spill_func = function
    | block::f' -> (spill_block [] block)::(spill_func f')
    | []        -> []
  in
    spill_func f

(* Adds the function progolue and epilogue *)
let make_prologue_and_epilogue (f: func) (spill_amt : int) =
  f

(* Perfomes a coalescing register allocation for the given func *)
let reg_alloc (f : func) : func = 
  let rec alloc f spill_amt =
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

          (* Mips.R29;
          Mips.R30; Mips.R31; Mips.R2; *)
        ]
        ColorSet.empty
    in
    let stack = simplify graph all_colors [] in
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
      | Success(map) -> 
          print_string (colorMap2string map); 
          make_prologue_and_epilogue (assign_registers f map) spill_amt
      | Fail(spill)  -> alloc (perform_spill f spill spill_amt) (spill_amt+1)
  in
    alloc f 0


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
              Mips.Add(l, l, op2mipsop r2)::a'
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

