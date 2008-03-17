type t
(** The type of event dispatchers *)

type event = Input | Priority | Output | Error | Hangup
(** An event associated with a file descriptor *)

type fd_handler = Unix.file_descr -> event list -> unit
(** [fd_handler d fd evt] handles an [event] generated by dispatcher [d] *)

type timer_handler = float -> unit
(** [timer_handler d when] is called at or after [when] by dispatcher [d] *)

val create : int -> t
(** Create a new event dispatcher, preallocating [size] fd events.  [size]
    is just a hint, the fd list will grow on demand. *)

val destroy : t -> unit
(** Destroy an event dispatcher *)

val add : t -> Unix.file_descr -> fd_handler -> event list -> unit
(** [add d fd handler events] begins listening for [events] on file
    descriptor [fd], calling [handler] when an event occurs. *)

val modify : t -> Unix.file_descr -> event list -> unit
(** [modify d fd events] changes the events to pay attention to on [fd] *)

val set_handler : t -> Unix.file_descr -> fd_handler -> unit
(** [set_handler d fd handler] changes the handler to be invoked for
    events on [fd] *)

val delete : t -> Unix.file_descr -> unit
(** [delete d fd] stops [d] paying attention to events on file
    descriptor [fd] *)

val add_timer : t -> timer_handler -> float -> unit
(** [add_timer d time handler] will cause dispatcher [d] to invoke
    [handler d time] at or after [time] *)

val delete_timer : t -> float -> unit
(** [delete_timer d time] prevents dispatcher from invoking any
    handlers added for [time] *)

val once : t -> unit
(** [once d] will dispatch one event (or set of simultaneous events)
    added to [d]. *)

val run : t -> unit
(** [run d] will dispatch events from [d] until all file descriptors
    have been removed and all timers have run or been removed *)
