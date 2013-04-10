let RunTests () =
    
    let engine = ref (fun x -> x) in
    let source_serializer = ref (fun x -> x) in
    let expected_serializer = ref (fun x -> x) in

    let success = ref true in
    
    let case source expected =
        try (
            let output = engine source in
                if output <> expected then (
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
                source 
                (Printexc.to_string ex);
        )

    (* TESTS GO HERE *)

    case "test" "test";
    case "test" "fail";

    source_serializer := print_func;
    expected_serializer := print_cfg;
    engine := build_cfg;

    case aFunc aCFG;
    case anotherFunc anotherCFG;

    (* ------------- *)
    
    if !success then 
        print_string "All Tests Passed.";