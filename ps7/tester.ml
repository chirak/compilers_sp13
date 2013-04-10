open Cfg
open Cfg_ast
open Interfere_graph

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
let m = Var "m"
let res = Var "result"

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
  (* Arith(res,j,Plus,k):: *)
  (* Move(Reg(Mips.R2), res):: *)
  Return::[]

let main_out : interfere_graph =
  List.fold_right
      graph_add
      [
        b, c;
        b, e;
        b, m;
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
      IGraphEdgeSet.empty

let test_block1 : block =
  [
    Label "L0";
    Arith(h, Int 10, Minus, Int 1);
    Arith(f, Int 9, Times, h);
    Jump "L1";
  ]

let test_block2 : block =
  [
    Label "L1";
    Load(h, f, 4);
    Move(k, h);
    Return;
  ]

let test_block3 : block =
  [
    Label "L0";
    Move(g, j);
    Move(h, k);
    If(g, Lt, h, "L1", "L2");
  ]

let test_block4 : block =
  [
    Label "L1";
    Move(k, g);
    Return;
  ]

let test_block5 : block =
  [
    Label "L2";
    Move(i, h);
    Move(k, j);
    Move(m, f);
    Move(j, f);
    Return;
  ]


let out3 : interfere_graph =
  List.fold_right
      graph_add
      [
        g, k;
        g, f;
        g, h;
        g, j;
        k, f;
        f, h;
        f, j;
        h, j;
      ]
      IGraphEdgeSet.empty


let runTests () =
    
    let success = ref true in
    
    let test engine source_serializer expected_serializer comparer source expected =
        try (
            let output = engine source in
                if not (comparer output expected) then (
                    success := false;
                    Printf.printf 
                        "TEST FAILED:\n%s\nExpected:\n%s\nActual:\n%s\n\n]" 
                        (source_serializer source) 
                        (expected_serializer expected)
                        (expected_serializer output);
                )
        )
        with ex -> (
            success := false;
            Printf.printf 
                "TEST CRASHED:\n%s\n%s" 
                (source_serializer source)
                (Printexc.to_string ex);
        )
    in

        (* TESTS GO HERE *)

        let engine f = build_interfere_graph (build_cfg f) in
        let source_serializer f = String.concat "" (List.map block2string f) in
        let expected_serializer = graph2string in
        let comparer = IGraphEdgeSet.equal in

        let case = test engine source_serializer expected_serializer comparer in

            case [main_block] main_out;
            case [test_block1; test_block2] IGraphEdgeSet.empty;
            case [test_block3; test_block4; test_block5] out3;

        (* ------------- *)
        
        if !success then
            print_string "All Tests Passed.\n";