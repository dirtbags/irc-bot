type event = Input | Priority | Output | Error | Hangup
type timer_handler = float -> unit
type fd_handler = Unix.file_descr -> event list -> unit

module Timer =
  Set.Make (struct
              type t = (float * timer_handler)
              let compare (time, handler) (time', handler') = compare time time'
            end)

module Fd_map =
  Map.Make (struct
              type t = Unix.file_descr
              let compare = compare
            end)

type t = {
  e : Epoll.t;
  fds : (fd_handler * event list) Fd_map.t ref;
  numfds : int ref;
  timers : Timer.t ref;
}

(* select(), poll(), and epoll() treat timeout as an upper bound of time
   to wait.  This fudge factor helps ensure that given no FD activity,
   this isn't run in a tight loop as a timer approaches.  This value was
   determined experimentally on a 1.25GHz G4 PPC to work most of the
   time.  Your mileage may vary. *)

let timeout_fudge = 0.001

let to_epoll = function
  | Input -> Epoll.In
  | Priority -> Epoll.Priority
  | Output -> Epoll.Out
  | Error -> Epoll.Error
  | Hangup -> Epoll.Hangup

let from_epoll = function
  | Epoll.In -> Input
  | Epoll.Priority -> Priority
  | Epoll.Out -> Output
  | Epoll.Error -> Error
  | Epoll.Hangup -> Hangup

let rec epoll_events_of_events = List.map to_epoll
  
let rec events_of_epoll_events = List.map from_epoll

let create size =
  {e = Epoll.create size;
   fds = ref Fd_map.empty;
   numfds = ref 0;
   timers = ref Timer.empty}

let destroy d =
  Epoll.destroy d.e;
  (* Explicitly unreference fds and timers, in case d sticks around *)
  d.fds := Fd_map.empty;
  d.numfds := 0;
  d.timers := Timer.empty

let add d fd handler events =
  Epoll.ctl d.e Epoll.Add (fd, (epoll_events_of_events events));
  d.fds := Fd_map.add fd (handler, events) !(d.fds);
  d.numfds := !(d.numfds) + 1

let modify d fd events =
  Epoll.ctl d.e Epoll.Modify (fd, (epoll_events_of_events events))

let set_handler d fd handler =
  let (_, events) = Fd_map.find fd !(d.fds) in
    d.fds := Fd_map.add fd (handler, events) !(d.fds)

let delete d fd =
  Epoll.ctl d.e Epoll.Delete (fd, []);
  d.fds := Fd_map.remove fd !(d.fds);
  d.numfds := !(d.numfds) - 1

let add_timer d handler time =
  d.timers := Timer.add (time, handler) !(d.timers)

let delete_timer d time =
  let may_remain (time', _) = 
    time' <> time 
  in
  d.timers := Timer.filter may_remain !(d.timers)


let rec dispatch_timers d now =
  if (!(d.timers) != Timer.empty) then
    let (time, handler) = Timer.min_elt !(d.timers) in
      if now < time then
        ()
      else begin
        handler time;
        d.timers := Timer.remove (time, handler) !(d.timers);
        dispatch_timers d now
      end

let rec dispatch_results d events_list =
  match events_list with
    | [] ->
        ()
    | (fd, epoll_events) :: tl ->
        let handler, _ = Fd_map.find fd !(d.fds) in
        let events = events_of_epoll_events epoll_events in
          handler fd events;
          dispatch_results d tl

let once d =
  let now = Unix.gettimeofday () in
  let timeout =
    try
      let (time, _) = Timer.min_elt !(d.timers) in
      let delta = (time -. now +. timeout_fudge) in
	max delta 0.0
    with Not_found ->
      (-1.0)
  in
    (if !(d.numfds) = 0 then
       (* epoll()--and probably poll()--barfs if it has no file descriptors *)
       ignore (Unix.select [] [] [] timeout)
     else
       (* poll() and epoll() wait *at most* timeout ms.  If you have fds but they're not
	  doing anything, multiple calls to once may be required.  This is lame. *)
       let timeout_ms = int_of_float (timeout *. 1000.0) in
       let result = Epoll.wait d.e !(d.numfds) timeout_ms in
	 dispatch_results d result);
    dispatch_timers d (Unix.gettimeofday ())

let rec run d =    
  if ((!(d.fds) == Fd_map.empty) &&
        (!(d.timers) == Timer.empty)) then
    ()
  else begin
    once d;
    run d
  end
