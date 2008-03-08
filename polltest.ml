external poll : 'a -> 'b -> 'c = "ocaml_poll"

type event = POLLIN | POLLPRI | POLLOUT | POLLERR | POLLHUP | POLLNVAL

let _ =
  poll [
    (10, [POLLIN; POLLOUT]);
    (20, [POLLOUT])
  ] ()
