type goob = Goob of (int ref) * int

let _ =
  let a = Goob (ref 1, 2) in
    if (match a with
          | Goob ({contents = 1}, _) -> true
          | _ -> false) then
      print_endline "hi asl"
