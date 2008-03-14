(*
 * OCaml epoll() interface
 * Author: Neale Pickett <neale@woozle.org>
 * Time-stamp: <2008-03-14 11:49:20 neale>
 *)

(**
 * This module provides an interface to epoll() on Linux, or poll() on
 * everything else.
 *)

type t

type event = In | Priority | Out | Error | Hangup
  (** Event types, mirroring poll() and epoll() event constants. *)

type op = Add | Modify | Delete
  (** Operations for ctl *)

external create : int -> t = "ocaml_epoll_create"
  (** Create a new poll structure *)

external destroy : t -> unit = "ocaml_epoll_destroy"
  (** Destroy a poll structure *)

external ctl : t -> op -> (Unix.file_descr * event list) -> unit = "ocaml_epoll_ctl"
  (** Add, Modify, or Delete an event list *)

external wait : t -> int -> int -> (Unix.file_descr * event list) list = "ocaml_epoll_wait"
(** [wait e maxevents timeout] returns a list of at most [maxevents]
    (file descriptor * event list)s that occurred before at least
    [timeout] milliseconds elapsed.
 *)
