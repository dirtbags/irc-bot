(* Buffered I/O, Scheme ports.  *)

open Ocs_error

type port_impl =
  | Input_channel of in_channel
  | Output_channel of out_channel
  | Input_string of string * int ref
  | Output_string of Buffer.t

type port = {
  ungot : char option ref;
  impl : port_impl;
}

let is_input p =
  match p.impl with
    | Input_string _
    | Input_channel _ -> true
    | _ -> false

let is_output p =
  match p.impl with
    | Output_string _
    | Output_channel _ -> true
    | _ -> false

let getc p =
  match (!(p.ungot), p.impl) with
    | (Some c, _) ->
        p.ungot := None;
        Some c
    | (None, Input_string (str, pos)) ->
        if !pos >= (String.length str) then
          None
        else
          let c = str.[!pos] in
            pos := !pos + 1;
            Some c
    | (None, Input_channel chan) ->
        begin
          try
            Some (input_char chan)
          with End_of_file ->
            None
        end
    | _ ->
        None

let flush p =
  match p.impl with
    | Output_channel chan ->
        Pervasives.flush chan
    | _ ->
        ()

let close p =
  p.ungot := None;
  match p.impl with
    | Input_channel chan ->
        close_in chan
    | Output_channel chan ->
        close_out chan
    | _ ->
        ()

let ungetc p c =
  p.ungot := Some c

let char_ready p =
  match (!(p.ungot), p.impl) with
    | (Some _, Input_string _)
    | (Some _, Input_channel _) ->
        true
    | (None, Input_string (str, pos)) ->
        !pos < (String.length str)
    | (None, Input_channel chan) ->
        let fd = Unix.descr_of_in_channel chan in
        let (r, _, _) = Unix.select [fd] [] [] 0.0 in
          List.length r > 0
    | _ ->
        false

let putc p c =
  match p.impl with
    | Output_string buf ->
        Buffer.add_char buf c
    | Output_channel chan ->
        output_char chan c
    | _ ->
        ()

let puts p s =
  match p.impl with
    | Output_string buf ->
        Buffer.add_string buf s
    | Output_channel chan ->
        output_string chan s
    | _ ->
        ()

let input_port ch =
  { ungot = ref None; impl = Input_channel ch }

let output_port ch =
  { ungot = ref None; impl = Output_channel ch }

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

let open_input_string s =
  { ungot = ref None; impl = Input_string (s, ref 0) }

let open_output_string () =
  { ungot = ref None; impl = Output_string (Buffer.create 256) }

let get_output_string p =
  match p.impl with
    | Output_string buf ->
        Buffer.contents buf
    | _ ->
        ""
