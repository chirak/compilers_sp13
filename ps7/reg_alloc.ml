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
        if not (IGraphEdgeSet.mem (InterfereEdge(l, r)) g) && can_coalesce combined then
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
    | None -> resolve_constraints g k stack

and resolve_constraints g k stack =
  let g' = 
    IGraphEdgeSet.filter
      (function
        | MoveEdge(l, r) 
          when IGraphEdgeSet.mem (InterfereEdge(l, r)) g -> 
            false
        | _ -> true)
      g
  in
    if IGraphEdgeSet.equal g g' then
      freeze g k stack
    else
      simplify g' k stack

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
  let pre_colored = 
    IGraphEdgeSet.fold
      (fun edge color_map ->
        match edge with InterfereEdge(l, r) | MoveEdge(l, r) ->
          let update m = function
            | Normal(Reg(r)) -> OperandMap.add (Reg(r)) r m
            | _ -> m
          in
            update (update color_map l) r)
      graph
      OperandMap.empty
  in
    match select graph stack all_colors pre_colored with
    | Success(map) -> assign_registers f map
    | Fail(spill)  -> reg_alloc (perform_spill f spill)


(* Finally, translate the ouptut of reg_alloc to Mips instructions *)
let cfg_to_mips (f : func) : Mips.inst list = 
  let op2mipsop = function
    | Int(i) -> Mips.Immed(Word32.fromInt i)
    | Reg(r) -> Mips.Reg(r)
    | _      -> raise Impossible
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

