open Ocs_types

let prefix_print prefix =
  function
    | [| Sstring txt |] ->
        print_endline (prefix ^ txt);
        Sunspec
    | _ ->
        raise (Ocs_error.Error "Invalid arguments")

let prefix_print_proc prefix =
  let primf = Pfn (prefix_print prefix) in
  let sprim = { prim_fun = primf; prim_name = "iobuf-write" } in
    Sprim sprim

let code =
  Capply1 ((Cval (prefix_print_proc "pfx: ")),
           (Cval (Sstring "hello world")))

let _ =
  let thread = Ocs_top.make_thread () in
  let outport = Ocs_port.open_output_string () in
    Ocs_eval.eval thread (Ocs_print.print outport false) code;
    Ocs_port.get_output_string outport
