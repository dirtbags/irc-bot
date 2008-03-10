(*
 * OCaml poll() / epoll() interface
 * Author: Neale Pickett <neale@woozle.org>
 * Time-stamp: <2008-03-10 16:38:38 neale>
 *)

(**
 * This module provides an interface to epoll() on Linux, or poll() on
 * everything else.  The provided interface is pretty raw, and if you're
 * not careful you can really booger things up.
 *)

type t

type event = In | Out | Pri | Err | Hup | Nval
  (** Event types, mirroring poll() and epoll() event constants.
   * Apparently Linux has some way to deal with Nval so that you never
   * see it. 
   *)

type op = Add | Modify | Delete
  (** Operations for ctl *)

type data
  (** User data type *)

external create : int -> t = "ocaml_poll_create"
  (* Create a new poll structure *)

external destroy : t -> unit = "ocaml_poll_destroy"
  (* Destroy a poll structure *)

external ctl : t -> op -> (Unix.file_descr * event list) -> unit = "ocaml_poll_ctl"
  (* Add, Modify, or Delete an event list and associated user data *)

external wait : t -> int -> (Unix.file_descr * event list) list = "ocaml_poll_wait"
  (* Block on events *)
