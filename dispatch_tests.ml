open OUnit

let unit =
  "Dispatch unit tests" >::: [
    "basic" >:: 
      (fun () ->
         let d = Dispatch.create 3 in
         let a,b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
         let rec handle fd events =
           match events with
             | [Dispatch.Input; Dispatch.Output] ->
                 let s = String.create 4096 in
                 let n = Unix.read fd s 0 4096 in
                   assert_equal
                     n
                     (Unix.write fd s 0 n)
             | _ ->
                 ()
         in
           assert_equal 2 (Unix.write a "hi" 0 2);
           Dispatch.add d b handle [Dispatch.Input; Dispatch.Output];
           Dispatch.once d;
           let s = String.create 4096 in
             assert_equal 2 (Unix.read a s 0 4096);
             assert_equal "hi" (Str.string_before s 2);

             Dispatch.destroy d;
             Unix.close a;
             Unix.close b
      );
  ]
