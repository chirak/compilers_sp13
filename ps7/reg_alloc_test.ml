open Cfg_ast
open Cfg
open Interfere_graph
open Reg_alloc
open Test_blocks

(* Bootstrap test framework which allows us to test a variety of different
 * things by providing:
 *  -engine: function(s) that you want to test
 *  -soruce_serializer: serialzer for input provided. Only used if test fails.
 *  -expected_serialzer: serializer for result. Only used if test fails.
 *  -comparer: comparison function to validate expected and actual result.
 *)

let make_graph_info =
  List.fold_left (fun m (k,v) -> NodeMap.add k v m) NodeMap.empty
let graph_info_2 =
  make_graph_info [(Normal a, 1); (Normal b, 1); (Normal c, 2)]
let graph_info_3 =
  make_graph_info [(Normal a, 2); (Normal b, 3); (Normal c, 2); (Normal d, 3)]
let graph_info_4 = 
  make_graph_info
    [(Normal f, 4);
     (Normal g, 4);
     (Normal h, 3);
     (Normal j, 4);
     (Normal k, 3);]

let main_graph_info = 
  make_graph_info
    [(Normal b, 3);
     (Normal c, 2);
     (Normal e, 4);
     (Normal f, 3);
     (Normal g, 3);
     (Normal h, 2);
     (Normal j, 5);
     (Normal k, 2);
     (Normal m, 4);
    ]

let main_graph_info_no_move = 
  make_graph_info
    [
     (Normal e, 4);
     (Normal f, 3);
     (Normal g, 3);
     (Normal h, 2);
     (Normal k, 2);
     (Normal m, 4);
    ]

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

  (* Test graphInfo creation from a given interference graph *)
  let engine f = get_info f in
  let source_serializer f = igraph2string f in
  let expected_serializer = graphInfo2string in
  let comparer = NodeMap.equal (=) in
  let case = test engine source_serializer expected_serializer comparer in
    
    case empty_igraph NodeMap.empty;
    case igraph_1     NodeMap.empty;
    case igraph_2     graph_info_2;
    case igraph_3     graph_info_3;
    case igraph_4     graph_info_4;
    case main_out     main_graph_info;

  (* Test move node removal from graph info *)
  let engine f = f in
  let source_serializer f = graphInfo2string f in
  let expected_serializer = graphInfo2string in
  let comparer = NodeMap.equal (=) in
  let case = test engine source_serializer expected_serializer comparer in
    case (remove_move_nodes graph_info_4 igraph_4) NodeMap.empty;
    case (remove_move_nodes main_graph_info main_out) main_graph_info_no_move;

  (* (* Test node removal from interference graph *) *)
  (* let engine f = f in *)
  (* let source_serializer f = igraph2string f in *)
  (* let expected_serializer = igraph2string in *)
  (* let comparer = IGraphEdgeSet.equal in *)
  (* let case = test engine source_serializer expected_serializer comparer in *)

    if !success then
      print_string "Reg Alloc Tests Passed.\n";
;;

let integration_test() =
  (* let func1 = [block_1a; block_1b] in *)
  let func3 = [block_3a; block_3b; block_3c] in
  let alloced_func1 = reg_alloc func3 in
    print_string (fun2string func3);
    print_string "\n----------------------------\n";
    print_string (fun2string alloced_func1);
;;

let _ = integration_test()
