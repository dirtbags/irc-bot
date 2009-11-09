type handler = Iobuf.t -> Command.t -> unit

let handlers = ref []

let register handler =
  handlers := !handlers @ [handler]

let unregister handler =
  handlers := List.filter ((<>) handler) !handlers

let handle_command iobuf cmd =
  let rec loop h =
    match h with
      | [] -> ()
      | handler :: tl ->
          begin
            try
              handler iobuf cmd
            with _ ->
              ()
          end;
          loop tl
  in
    loop !handlers
