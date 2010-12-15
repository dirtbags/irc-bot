let spawn prog args =
  let fd0_exit, fd0_entr = Unix.pipe () in
  let fd1_exit, fd1_entr = Unix.pipe () in
    match (Unix.fork ()) with
      | 0 ->                              (* Child *)
          Unix.dup2 fd0_exit Unix.stdin;
          Unix.close fd0_entr;
          Unix.close fd0_exit;

          Unix.dup2 fd1_entr Unix.stdout;
          Unix.close fd1_entr;
          Unix.close fd1_exit;

          Unix.execvp prog args

      | pid ->                            (* Parent *)
          Unix.close fd0_exit;
          Unix.close fd1_entr;

          (fd0_entr, fd1_exit)
  

let create d text input_handler output_handler prog args =
  let child_stdin, child_stdout = spawn prog args in
    Dispatch.add d child_stdin output_handler [Dispatch.Output];
    Dispatch.add d child_stdout input_handler [Dispatch.Input]


(** Canned process: sends a string on stdin, collects stdout and stderr,
    and calls a callback when everything's finished. *)

type canned = {
  finished : string -> unit;
  stdin : string;
  stdout : string;
  stderr : string;
  mutable stdin_pos : int;
  mutable stdout_pos : int;
  mutable stderr_pos : int;
}

let canned_handler d p fd event =
  match event with
    | Dispatch.Input ->
        let len =
          Unix.read fd p.stdout p.stdout_pos
            ((String.length p.stdout) - p.stdout_pos)
        in
          if (len > 0) then
            p.stdout_pos <- p.stdout_pos + len
          else begin
            Dispatch.delete d fd;
            p.finished (String.sub p.stdout 0 p.stdout_pos)
          end
    | Dispatch.Output ->
        begin
          try
            let len =
              Unix.write fd p.stdin p.stdin_pos
                ((String.length p.stdin) - p.stdin_pos)
            in
              p.stdin_pos <- p.stdin_pos + len;
              if (p.stdin_pos == String.length p.stdin) then begin
                Unix.close fd;
                Dispatch.delete d fd
              end
          with Unix.Unix_error _ ->
            Unix.close fd;
            Dispatch.delete d fd
        end
    | Dispatch.Exception ->
        ()

let create_canned d text finished prog args =
  let p =
    {
      finished=finished;
      stdin=text; stdin_pos=0;
      stdout=String.create 8192; stdout_pos=0;
      stderr=String.create 8192; stderr_pos=0;
    }
  in
  let handler = (canned_handler d p)
  in
    create d text handler handler prog args






(** Zombie reapin' mayhem *)
let rec sigchld s =
  try
    match Unix.waitpid [Unix.WNOHANG] (-1) with
      | (0, _) ->
          ()
      | _ ->
          sigchld s
  with Unix.Unix_error (Unix.ECHILD, _, _) ->
    ()

let sigpipe s = ()

let _ =
  Sys.set_signal Sys.sigchld (Sys.Signal_handle sigchld);
  Sys.set_signal Sys.sigpipe (Sys.Signal_handle sigpipe)

