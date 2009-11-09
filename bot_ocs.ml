open Ocs_types

module Iobuf =
  Ocs_wrap.Make (struct
                   type t = Iobuf.t
                 end)

let rec string_list_of_spair v acc =
  match v with
    | Snull ->
        acc
    | Spair { car = Sstring s; cdr = cdr } ->
        string_list_of_spair cdr (acc @ s)
    | _ ->
        raise Ocs_error.Error "Not a string list"

let write_vals iobuf vals =
  let cmd =
    match vals with
      | [| Swrapped iobuf; Sstring command; Spair args; Sstring text |] ->
          Command.create None command (string_list_of_spair args) (Some text)
      | [| Swrapped iobuf; Sstring command; Spair args |] ->
          Command.create None command (string_list_of_spair args) None
      | _ ->
          raise Ocs_error.Error "Invalid arguments"
  in
    Iobuf.write iobuf cmd

let iobuf_write_proc iobuf =
  let primf = Pfn (write_vals iobuf) in
  let sprim = { prim_fun = primf; prim_name = "iobuf-write" } in
    Sproc (sprim, [| [| |] |])

let ocs_bind b regexp cb =
  match (regexp, cb) with
    | (Sstring regexp_s, Sproc (p, d)) ->
        let regexp = Str.regexp regexp_s in
          b := Bindings.add (regexp_s, regexp, p)
    | _ ->
        raise Ocs_error.Error "invalid arguments"

let init b e =
  set_pf3 e (ocs_bind b) "bind"

