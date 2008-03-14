type fd_handler = t -> event -> Unix.file_descr -> unit
type timeout_handler = t -> float -> unit
type timeout = (float * timeout_handler)

module Fd_map = 
    Map.Make (struct
		type t = Unix.file_descr
		let compare = compare
	      )

module Timeout_set =
  Set.Make (struct
	      type t = timeout
	      let compare = compare
	    )

type event = Input | Priority | Output | Error | Hangup

type t = {
  e : Epoll.t;
  fds : (fd_handler * event list) Fd_map.t ref;
  numfds : int ref;
  timeouts : timeout_handler Timeout_set.t ref;
}

let rec epoll_events_of_events 
  | [] -> []
  | Input :: tl -> Epoll.In @ (epoll_events_of_events tl)
  | Priority :: tl -> Epoll.Priority @ (epoll_events_of_events tl)
  | Output :: tl -> Epoll.Output @ (epoll_events_of_events tl)
  | Error :: tl -> Epoll.Error @ (epoll_events_of_events tl)
  | Hangup :: tl -> Epoll.Hangup @ (epoll_events_of_events tl)
  
let rec events_of_epoll_events
  | [] -> []
  | Epoll.In :: tl -> Input @ (events_of_epoll_events)
  | Epoll.Priority :: tl -> Priority @ (events_of_epoll_events)
  | Epoll.Out :: tl -> Out @ (events_of_epoll_events)
  | Epoll.Error :: tl -> Error @ (events_of_epoll_events)
  | Epoll.Hangup :: tl -> Hangup @ (events_of_epoll_events)

let create size =
  {e = Epoll.create size;
   fds = ref Fd_map.empty;
   numfds = ref 0;
   timeouts = ref Timeout_set.empty}

let destroy d =
  Epoll.destroy d.e;
  (* Explicitly unreference fds and timeouts, in case d sticks around *)
  d.fds := Fd_map.empty;
  d.numfds := 0;
  d.timeouts := Timeout_set.empty

let add d fd handler events =
  Epoll.ctl d.e Epoll.Add (fd, (epoll_events_of_events events));
  d.fds := Fd_map.add fd (handler, events) !d.fds;
  d.numfds := !d.numfds + 1

let modify d fd events =
  Epoll.ctl d.e Epoll.Modify (fd, (epoll_events_of_events events))

let set_handler d fd handler =
  let (_, events) = Fd_map.find fd in
    d.fds := Fd_map.add fd (handler, events) !d.fds

let delete d fd =
  Epoll.ctl d.e Epoll.Delete (fd, []);
  d.fds := Fd_map.remove fd !d.fds;
  d.numfds := !d.numfds - 1

let add_timeout d time handler =
  d.timeouts := Timeout_set.add (time, handler) !d.timeouts

let delete d time =
  let may_remain (time', _) = 
    time' <> time 
  in
  d.timeouts := Timeout_set.filter may_remain !d.timeouts


let rec dispatch_timeouts d now =
  let (time, handler) = Timeout_set.min_elt !d.timeouts in
    if now > time then
      ()
    else begin
      handler d time;
      d.timeouts := Timeout_set.remove time !d.timeouts;
      dispatch_timeouts d now
    end

let rec dispatch_events d events_list =
  match events_list with
    | [] ->
	()
    | (fd, epoll_events) :: tl ->
	let handler = Fd_map.find fd !d.fds in
	let events = events_of_epoll_events in
	  handler d fd events;
	  dispatch_events d tl

let once d =
  let now = Unix.time () in
  let timeout =
    try
      let (time, _) = Timeout_set.min_elt !d.timeouts in
      let timeout_s = max (time - now) 0.0 in
	int_of_float (timeout_s *. 1000.0)
    with Not_found ->
      -1
  in
  let result = Epoll.wait d.e !d.nfds timeout in
    dispatch_timeouts d (Unix.time ());
    dispatch_events d result

let rec run d =    
  if ((!d.fds == Fd_map.empty) &&
	(!d.timeouts == Timeout_set.empty)) then
    ()
  else begin
    once d;
    run d
  end

	
