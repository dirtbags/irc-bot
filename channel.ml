type t = {name: string}

let modes = "t"

let by_name = Hashtbl.create 25

let lookup name =
  Hashtbl.find by_name name

let create name =
  {name = name}

let is_channel_name name =
  match name.[0] with
    | '#' | '&' | '!' | '+' ->
        true
    | _ ->
        false
  
