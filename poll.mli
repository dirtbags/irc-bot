type pollfd = {fd: int;
               events: int;
               revents: int}

external in : int = "poll_in"
