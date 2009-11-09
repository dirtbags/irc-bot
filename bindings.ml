type callback = Iobuf.t -> Command.t -> unit
type t = (string * Str.regexp * callback) list

let create () = []

let remove b id =
  let keep = function
    | (id', _, _) ->
        id' <> id
  in
    List.filter keep b

let add b id regex cb =
  (id, regex, cb) :: (remove b id)

let lookup b text =
  let rec groups str i acc =
    try
      groups str (i + 1) ((Str.matched_group i str) :: acc)
    with
      | Not_found ->
          groups str (i + 1) ("" :: acc)
      | Invalid_argument _ ->
          List.rev acc
  in
  let rec loop b acc =
    match b with 
      | [] -> 
          List.rev acc
      | (id, regex, cb) :: tl ->
          try
            ignore (Str.search_forward regex text 0);
            loop tl ((id, cb, groups text 0 []) :: acc)
          with Not_found ->
            loop tl acc
  in
    loop b []

    

