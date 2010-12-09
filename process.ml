type t = {
  
}

let create iobuf prog args =
  let fd0_exit, fd0_entr = Unix.pipe () in
  let fd1_exit, fd1_entr = Unix.pipe () in
  let fd2_exit, fd2_entr = Unix.pipe () in
  let pid = Unix.create_process prog args fd0_exit fd1_entr fd2_entr in
  

let handle_event process fd event =
  match event with
    | Dispatch.Input ->
      
