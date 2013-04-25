open Cfg
open Cfg_ast
open Interfere_graph

let r1 = Reg Mips.R1
let r2 = Reg Mips.R2
let r3 = Reg Mips.R3
let a = Var "a"
let b = Var "b"
let c = Var "c"
let d = Var "d"
let e = Var "e"
let f = Var "f"
let g = Var "g"
let h = Var "h"
let i = Var "i"
let j = Var "j"
let k = Var "k"
let l = Var "l"
let m = Var "m"
let res = Var "result"

let add_interfere_edges =
  List.fold_left (fun a (x, y) -> graph_add (Normal(x), Normal(y)) E_Interfere a) IGraphEdgeSet.empty
let add_move_edges g =
  List.fold_left (fun a (x, y) -> graph_add (Normal(x), Normal(y)) E_Move a) g

let block_0a : block =
  [
    Label "L0";
    Arith(h, Int 10, Minus, Int 1);
    Arith(f, Int 9, Times, h);
    Return;
  ]

(* ------------------------------------------ *)

let block_1a : block =
  [
    Label "L0";
    Arith(h, Int 10, Minus, Int 1);
    Arith(f, Int 9, Times, h);
    Jump "L1";
  ]

let block_1b : block =
  [
    Label "L1";
    Load(h, f, 4);
    Move(k, h);
    Return;
  ]

let igraph_1 = add_move_edges IGraphEdgeSet.empty [(k, h)]

(* ------------------------------------------ *)

let block_2a : block =
  [ 
    Label "L0";
    Move(a, Int 0);
    Jump "L1"
  ]

let block_2b : block = 
  [
    Label "L1";
    Arith(b, a, Plus, Int 1);
    Arith(c, c, Plus, b);
    Arith(a, b, Times, Int 2);
    If(a, Lt, Int 9, "L1", "L2");
  ]

let block_2c : block =
  [
    Label "L2";
    Return;
  ]

let igraph_2 : interfere_graph = add_interfere_edges [(c, a); (c, b)]

(* ------------------------------------------ *)

let block_3a : block = 
  [
    Label "L0";
    Move(a, Int 0);
    Move(b, Int 5);
    Move(d, Int 4);
    Move(f, Int 100);
    If(a, Lt, b, "L1", "L2")
  ]

let block_3b : block = 
  [
    Label "L1";
    Arith(c, a, Plus, b);
    Move(d, Int 2);
    Return
  ]

let block_3c : block =
  [
    Label "L2";
    Move(c, Int 4);
    Arith(b, b, Times, d);
    Arith(b, b, Plus, c);
    Return;
  ]

let igraph_3 : interfere_graph =
  add_interfere_edges
      [
        a, b;
        a, d;
        b, c;
        b, d;
        c, d;
      ]

(* ------------------------------------------ *)

let block_4a : block =
  [
    Label "L0";
    Move(g, j);
    Move(h, k);
    If(g, Lt, h, "L1", "L2");
  ]

let block_4b : block =
  [
    Label "L1";
    Move(k, g);
    Return;
  ]

let block_4c : block =
  [
    Label "L2";
    Move(i, h);
    Move(k, j);
    Move(m, f);
    Move(j, f);
    Return;
  ]

let interfere_edges_4 =
  [ 
    f,g;
    f,h;
    f,j;
    f,k;
    g,h;
    g,j;
    g,k;
    h,j;
    j,k; 
  ]

let move_edges_4 =
  [ 
    g,j;
    g,k;
    h,k;
    i,h;
    j,f;
    k,j;
    m,f
  ]
let igraph_4 : interfere_graph =
  add_move_edges (add_interfere_edges interfere_edges_4) move_edges_4


(* ------------------------------------------ *)


let block_5a : block =
  [
    Label "f";
    Move(c, r3);
    Move(a, r1);
    Move(b, r2);
    Move(d, Int 0);
    Move(e, a);
    Jump "loop"
  ]
let block_5b : block =
  [
    Label "loop";
    Arith(d, d, Plus, b);
    Arith(e, e, Minus, Int 1);
    If(e, Lt, Int 0, "loop", "end");
  ]

let block_5c : block =
  [
    Label "end";
    Move(r1, d);
    Move(r3, c);
    Return;
  ]


(* ------------------------------------------ *)


let main_block : block =
  Label "L0"::
  Load(g,j,12)::
  Arith(h,k,Minus,Int 1)::
  Arith(f,g,Times,h)::
  Load(e,j,8)::
  Load(m,j,16)::
  Load(b,f,0)::
  Arith(c,e,Plus,Int 8)::
  Move(d,c)::
  Arith(k,m,Plus,Int 4)::
  Move(j,b)::
  Return::[]

let main_interfere_edges = 
  [
    b, e;
    b, m;
    b, c;
    e, f;
    e, j;
    f, j;
    g, h;
    g, k;
    j, g;
    j, h;
    j, k;
    m, c;
    m, e;
    m, f;
  ]

let main_move_edges = [c,d; b,j]

let main_out : interfere_graph =
  add_move_edges (add_interfere_edges main_interfere_edges) main_move_edges

