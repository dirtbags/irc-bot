/** OCaml poll() interface
 *
 * Time-stamp: <2008-03-10 17:19:26 neale>
 *
 * Copyright (C) 2008 Neale Pickett
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#ifdef __linux
#  include <sys/epoll.h>
#  define EPOLL
#  define POLL_IN EPOLLIN
#  define POLL_PRI EPOLLPRI
#  define POLL_OUT EPOLLOUT
#  define POLL_ERR EPOLLERR
#  define POLL_HUP EPOLLHUP
#  define POLL_NVAL 0
#else
#  include <poll.h>
#  undef EPOLL
#  define POLL_IN POLLIN
#  define POLL_PRI POLLPRI
#  define POLL_OUT POLLOUT
#  define POLL_ERR POLLERR
#  define POLL_HUP POLLHUP
#  define POLL_NVAL POLLNVAL
#endif

#include <string.h>
#include <errno.h>
#include <stdlib.h>

#define puke()                                                          \
  {                                                                     \
    char errstr[512];                                                   \
    snprintf(errstr, sizeof(errstr), __FUNCTION__ ": %s", strerror(errno)); \
    caml_failwith(errstr);                                              \
  }

enum {
  caml_POLLIN,
  caml_POLLPRI,
  caml_POLLOUT,
  caml_POLLERR,
  caml_POLLHUP,
  caml_POLLNVAL
};

enum {
  caml_POLL_ADD,
  caml_POLL_MOD,
  caml_POLL_DEL
};


static int
list_length(value list)
{
  int   len = 0;
  value l;

  for (l = list; l != caml_Val_int(0); l = caml_Field(l, 1)) {
    len += 1;
  }
  return len;
}

static int
int_of_event_list(value list)
{
  int   acc = 0;
  value l;

  for (l = list; l != caml_Val_int(0); l = caml_Field(l, 1)) {
    switch (caml_Int_val(caml_Field(l, 0))) {
      case caml_POLLIN:
        acc |= POLL_IN;
        break;
      case caml_POLLPRI:
        acc |= POLL_PRI;
        break;
      case caml_POLLOUT:
        acc |= POLL_OUT;
        break;
      case caml_POLLERR:
        acc |= POLL_ERR;
        break;
      case caml_POLLHUP:
        acc |= POLL_HUP;
        break;
      case caml_POLLNVAL:
        acc |= POLL_NVAL;
        break;
    }
  }
  return acc;
}

static value
append(value list, value item)
{
  value new = alloc_small(2, 0);
  caml_Field(new, 0) = item;
  caml_Field(new, 1) = list;
  return new;
}

static value
event_list_of_int(int events)
{
  value result = caml_Val_int(0);

  if (events & POLL_IN) {
    result = append(result, caml_Val_int(caml_POLLIN));
  } else if (events & POLL_PRI) {
    result = append(result, caml_Val_int(caml_POLLPRI));
  } else if (events & POLL_OUT) {
    result = append(result, caml_Val_int(caml_POLLOUT));
  } else if (events & POLL_ERR) {
    result = append(result, caml_Val_int(caml_POLLERR));
  } else if (events & POLL_HUP) {
    result = append(result, caml_Val_int(caml_POLLHUP));
  } else if (events & POLL_NVAL) {
    result = append(result, caml_Val_int(caml_POLLNVAL));
  }
  inspect_block(result);
  return result;
}

#ifdef EPOLL
/********************************************************************************
 *
 * epoll()
 *
 ********************************************************************************/
CAMLprim value
ocaml_poll_create(value size)
{
  CAMLparam1(size);
  CAMLlocal1(result);

  int ret;

  ret = epoll_create(caml_Int_val(size));
  if (-1 == ret) {
    puke();
  }
  result = caml_Val_int(ret);
  CAMLreturn(result);
}

CAMLprim value
ocaml_poll_destroy(value caml_t)
{
  CAMLparam1(caml_t);

  int ret;

  ret = close(caml_Int_val(caml_t));
  if (-1 == ret) {
    puke();
  }
  CAMLreturn(caml_Val_unit);
}

/* I'm sad that I can't do anything more interesting with evt.data than
 * put the file descriptor in it.  Like, say, storing a continuation.
 * Unfortunately doing so would require caml_register_global_root for
 * each one to prevent the GC from blowing them away or heap compaction
 * from moving them, and I don't want to do that for so many objects
 * (plus I'd (maybe) have to keep track of them).  Anyway this isn't so
 * bad, you can make a record type with the fd in it and search through
 * that pretty quickly with the balanced binary tree provided by Set, or
 * use a hash table if you like those better. */
CAMLprim value
ocaml_poll_ctl(value caml_t, value caml_op, value caml_what)
{
  CAMLparam3(caml_t, caml_op, caml_what);

  int op;
  int fd;
  int ret;
  struct epoll_event evt;

  switch (caml_Int_val(caml_op)) {
    case caml_POLL_ADD:
      op = EPOLL_CTL_ADD;
      break;
    case caml_POLL_MOD:
      op = EPOLL_CTL_MOD;
      break;
    case caml_POLL_DEL:
      op = EPOLL_CTL_DEL;
      break;
  }
  fd = caml_Int_val(Field(caml_what, 0));
  evt.events = int_of_event_list(caml_Field(f, 1));
  evt.data.fd = fd;

  ret = epoll_ctl(caml_Int_val(caml_t), op, fd, *evt);
  if (-1 == ret) {
    puke();
  }
  CAMLreturn(caml_Val_unit);
}

CAMLprim value
ocaml_poll_wait(value caml_t, value caml_timeout)
{
  CAMLparam2(caml_t, caml_timeout);
  CAMLlocal1(result);

  /* XXX: complete me */
  caml_enter_blocking_section();
  /* XXX: fix me */
  ret = epoll_wait(caml_Int_val(caml_t), fds, nfds, timeout);
  caml_leave_blocking_section();

  CAMLreturn(result);
}

#else
/********************************************************************************
 *
 * epoll()
 *
 ********************************************************************************/

CAMLprim value
ocaml_poll(value caml_fds, value caml_timeout)
{
  CAMLparam2(caml_fds, caml_timeout);
  CAMLlocal2(result, l);

  int           nfds;
  int           timeout = caml_Int_val(caml_timeout);
  int           i;
  int           ret;

  result = caml_Val_int(0);

  /* Count entries */
  nfds = list_length(caml_fds);

  {
    struct pollfd fds[nfds];

    /* Build fds */
    for (i=0, l = caml_fds; l != caml_Val_int(0); i += 1, l = caml_Field(l, 1)) {
      value f = caml_Field(l, 0);
      struct pollfd *p = &(fds[i]);

      p->fd = caml_Int_val(caml_Field(f, 0));
      p->events = int_of_event_list(caml_Field(f, 1));
    }

    /* Call poll */
    caml_enter_blocking_section();
    ret = poll(fds, nfds, timeout);
    caml_leave_blocking_section();
    if (-1 == ret) {
      puke();
    }

    for (i=0; i < nfds; i += 1) {
      struct pollfd *p = &(fds[i]);

      if (p->revents) {
        value v = alloc_small(2,0);

        caml_Field(v, 0) = caml_Val_int(p->fd);
        caml_Field(v, 1) = event_list_of_int(p->revents);
        result = append(result, v);
      }
    }

    CAMLreturn(result);
  }
}

#endif
