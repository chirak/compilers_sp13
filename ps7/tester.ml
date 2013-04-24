open Cfg
open Cfg_ast
open Interfere_graph
open Test_blocks

(* Bootstrap test framework which allows us to test a variety of different
 * things by providing:
 *  -engine: function(s) that you want to test
 *  -soruce_serializer: serialzer for input provided. Only used if test fails.
 *  -expected_serialzer: serializer for result. Only used if test fails.
 *  -comparer: comparison function to validate expected and actual result.
 *)
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

  let engine f = build_interfere_graph (build_cfg f) in
  let source_serializer f = String.concat "" (List.map block2string f) in
  let expected_serializer = igraph2string in
  let comparer = IGraphEdgeSet.equal in

  let case = test engine source_serializer expected_serializer comparer in
(*
    (*
     * ---------------------------------------------------------------------- *
     * Simple base case there should be no interference between h and f.
     * ---------------------------------------------------------------------- *
     *    L0:
     *    h := 10 - 1
     *    f := 9 * h
     *    Return;
     *)
    case [block_0a] IGraphEdgeSet.empty;


    (*
     * ---------------------------------------------------------------------- *
     * Another base case where there should be no interference accross two
     * blocks.
     * ---------------------------------------------------------------------- *
     *    L0:
     *    h := 10 - 1
     *    f := 9 * h
     *    Jump "L1"
     *
     *    L1:
     *    h := *(f+4)
     *    k := h
     *    Return
     *)
    case [block_1a; block_1b] IGraphEdgeSet.empty;

    (*
     * ---------------------------------------------------------------------- *
     * Test for a loop structured CFG. Variable "c" is alive throughout the loop
     * so it alwasy interferes with "a" and "b", but "a" and "b" never interfere
     * with each other.
     * ---------------------------------------------------------------------- *
     *    L0:
     *    a := 0
     *    Jump "L1"
     *
     *    L1:
     *    b := a + 1
     *    c := c + b
     *    a := b * 2
     *    if a < 9 then L1 else L2
     *
     *    L2:
     *    Return
     *)
    case [block_2a; block_2b; block_2c] igraph_2;

    (*
     * ---------------------------------------------------------------------- *
     * More complex case with unused variables. "f" is never used and should not
     * be in graph. "a" is only used in "L1".
     * ---------------------------------------------------------------------- *
     *    L0:
     *    a := 0
     *    b := 5
     *    c := 4
     *    f := 100
     *    if a < b then L1 else L2
     *
     *    L1:
     *    c := a + b
     *    d := 2
     *    Return
     *
     *    L2:
     *    c := 4
     *    b := b * d
     *    b := b + c
     *    Return
     *)
    case [block_3a; block_3b; block_3c] igraph_3;

    (*
     * ---------------------------------------------------------------------- *
     * Similar case as above except we have variable "f" that is never defined
     * in the program but it used in "L2" This should create interference for 
     * variables in the parent block "L0"
     * ---------------------------------------------------------------------- *
     *    L0:
     *    g := j
     *    h := k
     *    if g < h then L1 else L2
     *
     *    L1:
     *    k := g
     *    Return
     *
     *    L2:
     *    i := h
     *    k := j
     *    m := f
     *    j := f
     *    Return
     *)
    case [block_4a; block_4b; block_4c] igraph_4;
*)
    (*
     * ---------------------------------------------------------------------- *
     * Case from register allocation lecture slides. Creates the most
     * interference than any other example we have.
     * ---------------------------------------------------------------------- *
     *    L0:
     *    g := *(j+12)
     *    h := k - 1
     *    f := g * h
     *    e := *(j+8)
     *    m := *(j+16)
     *    b := *(f+0)
     *    c := e + 8
     *    k := m + 4
     *    j := b
     *    Return
     *)
    (* case [main_block] main_out; *)
    case [block_5a; block_5b; block_5c] IGraphEdgeSet.empty;

    if !success then
      print_string "All Tests Passed.\n";
;;

let _ = runTests ()

