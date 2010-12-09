type event = Input | Output | Exception
type timer_handler = float -> unit
type fd_handler = Unix.file_descr -> event -> unit

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
  read_fds : Unix.file_descr list ref;
  write_fds : Unix.file_descr list ref;
  except_fds : Unix.file_descr list ref;
  handlers : fd_handler Fd_map.t ref;
  timers : Timer.t ref;
}

let create ?(size=5) () =
  {read_fds = ref [];
   write_fds = ref [];
   except_fds = ref [];
   handlers = ref Fd_map.empty;
   timers = ref Timer.empty}

let destroy d =
  (* Explicitly unreference fds and timers, in case d sticks around *)
  d.handlers := Fd_map.empty;
  d.timers := Timer.empty

let get_fds d event =
  match event with
    | Input -> d.read_fds
    | Output -> d.write_fds
    | Exception -> d.except_fds

let modify d fd events =
  let add_event event =
    let l = get_fds d event in
    let nl = (List.filter ((<>) fd) !l) in
      if List.mem event events then
        l := fd :: nl
      else
        l := nl
  in
    if Fd_map.mem fd !(d.handlers) then
      List.iter add_event [Input; Output; Exception]
    else
      raise Not_found

let set_handler d fd handler =
  d.handlers := Fd_map.add fd handler !(d.handlers)

let add d fd handler events =
  set_handler d fd handler;
  modify d fd events

let delete d fd =
  let del_event event =
    let l = get_fds d event in
      l := (List.filter ((<>) fd) !l)
  in
    d.handlers := Fd_map.remove fd !(d.handlers);
    List.iter del_event [Input; Output; Exception]

let add_timer d handler time =
  d.timers := Timer.add (time, handler) !(d.timers)

let delete_timer d time =
  let may_remain (time', _) = 
    time' <> time 
  in
  d.timers := Timer.filter may_remain !(d.timers)


let rec dispatch_timers d now =
  if not (Timer.is_empty !(d.timers)) then
    let (time, handler) = Timer.min_elt !(d.timers) in
      if now < time then
        ()
      else begin
        handler time;
        d.timers := Timer.remove (time, handler) !(d.timers);
        dispatch_timers d now
      end

let rec dispatch_results d (read_ready, write_ready, except_ready) =
  let rec dispatch event fd_list =
    match fd_list with
      | [] ->
          ()
      | fd :: tl ->
          let handler = Fd_map.find fd !(d.handlers) in
            handler fd event;
            dispatch event tl
  in
    dispatch Input read_ready;
    dispatch Output write_ready;
    dispatch Exception except_ready

let once d =
  (* You might think it'd work better to use the timeout of select().
     Not so!  select() waits *at most* timeout ms.  Doing things
     this way results in a tight loop as the timer approaches. *)
  let interval = 
    try
      let (next, _) = Timer.min_elt !(d.timers) in
      let delta = (next -. (Unix.gettimeofday ())) in
	max delta 0.0
    with Not_found ->
      0.0
  in
  let s = { Unix.it_interval = interval; Unix.it_value = 0.0 } in
  let _ = Sys.set_signal Sys.sigalrm Sys.Signal_ignore in
  let _ = Unix.setitimer Unix.ITIMER_REAL s in
  let result = Unix.select !(d.read_fds) !(d.write_fds) !(d.except_fds) (-1.0) in
    dispatch_results d result;
    dispatch_timers d (Unix.gettimeofday ())

let rec run d =
  if (Fd_map.is_empty !(d.handlers)) && (Timer.is_empty !(d.timers)) then
    ()
  else begin
    once d;
    run d
  end

