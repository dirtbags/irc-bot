(* Buffered I/O, Scheme ports.  *)

open Ocs_error

type port =
  | Input_channel of in_channel * char option ref
  | Output_channel of out_channel
  | Input_string of (string * int ref) * char option ref
  | Output_string of Buffer.t

let is_input p =
  match p with
    | Input_string _
    | Input_channel _ -> true
    | _ -> false

let is_output p =
  match p with
    | Output_string _
    | Output_channel _ -> true
    | _ -> false

let getc p =
  match p with
    | Input_channel (_, ({contents = Some c} as ungot))
    | Input_string (_, ({contents = Some c} as ungot)) ->
        ungot := None;
        Some c
    | Input_channel (chan, {contents = None}) ->
        begin
          try
            Some (input_char chan)
          with End_of_file ->
            None
        end
    | Input_string ((str, pos), {contents = None}) ->
        if !pos >= (String.length str) then
          None
        else
          let c = str.[!pos] in
            pos := !pos + 1;
            Some c
    | _ ->
        None

let flush p =
  match p with
    | Output_channel chan ->
        Pervasives.flush chan
    | _ ->
        ()

let close p =
  match p with
    | Input_channel (chan, ungot) ->
        ungot := None;
        close_in chan
    | Output_channel chan ->
        close_out chan
    | _ ->
        ()

let ungetc p c =
  match p with
    | Input_channel (_, ungot)
    | Input_string (_, ungot) ->
        ungot := Some c
    | _ ->
        ()

let char_ready p =
  match p with
    | Input_string (_, {contents = Some _})
    | Input_channel (_, {contents = Some _}) ->
        true
    | Input_string ((str, pos), {contents = None}) ->
        !pos < (String.length str)
    | Input_channel (chan, {contents = None}) ->
        let fd = Unix.descr_of_in_channel chan in
        let (r, _, _) = Unix.select [fd] [] [] 0.0 in
          List.length r > 0
    | _ ->
        false

let putc p c =
  match p with
    | Output_string buf ->
        Buffer.add_char buf c
    | Output_channel chan ->
        output_char chan c
    | _ ->
        ()

let puts p s =
  match p with
    | Output_string buf ->
        Buffer.add_string buf s
    | Output_channel chan ->
        output_string chan s
    | _ ->
        ()

let input_port ch = Input_channel (ch, ref None)

let output_port ch = Output_channel ch

let open_input_port name =
  try
    input_port (open_in_bin name)
  with Sys_error err ->
    raise (Error ("unable to open '" ^ name ^ "' for input: " ^ err))

let open_output_port name =
  try
    output_port (open_out_bin name)
  with Sys_error err ->
    raise (Error ("unable to open '" ^ name ^ "' for output: " ^ err))

let open_input_string s = Input_string ((s, ref 0), ref None)

let open_output_string () =Output_string (Buffer.create 256)

let get_output_string p =
  match p with
    | Output_string buf ->
        Buffer.contents buf
    | _ ->
        ""
