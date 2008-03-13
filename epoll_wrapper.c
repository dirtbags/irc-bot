/** OCaml poll() interface
 *
 * Time-stamp: <2008-03-12 23:20:54 neale>
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
#else
#  include <poll.h>
#  undef EPOLL
#  define EPOLLIN POLLIN
#  define EPOLLPRI POLLPRI
#  define EPOLLOUT POLLOUT
#  define EPOLLERR POLLERR
#  define EPOLLHUP POLLHUP
#endif

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>

#define puke()                                                          \
  {                                                                     \
    char errstr[512];                                                   \
    snprintf(errstr, sizeof(errstr), "%s: %s", __FUNCTION__, strerror(errno)); \
    caml_failwith(errstr);                                              \
  }

enum {
  caml_POLLIN,
  caml_POLLPRI,
  caml_POLLOUT,
  caml_POLLERR,
  caml_POLLHUP
};

enum {
  caml_POLL_ADD,
  caml_POLL_MOD,
  caml_POLL_DEL
};


static int
list_length(value list)
{
  CAMLparam1(list);
  CAMLlocal1(l);

  int   len = 0;

  for (l = list; l != Val_int(0); l = Field(l, 1)) {
    len += 1;
  }
  CAMLreturn(len);
}

static int
int_of_event_list(value list)
{
  CAMLparam1(list);
  CAMLlocal1(l);

  int acc = 0;

  for (l = list; l != Val_int(0); l = Field(l, 1)) {
    switch (Int_val(Field(l, 0))) {
      case caml_POLLIN:
        acc |= EPOLLIN;
        break;
      case caml_POLLPRI:
        acc |= EPOLLPRI;
        break;
      case caml_POLLOUT:
        acc |= EPOLLOUT;
        break;
      case caml_POLLERR:
        acc |= EPOLLERR;
        break;
      case caml_POLLHUP:
        acc |= EPOLLHUP;
        break;
    }
  }
  CAMLreturn(acc);
}

static value
cons(value item, value list)
{
  CAMLparam2(item, list);
  CAMLlocal1(new);

  new = alloc_small(2, 0);

  Field(new, 0) = item;
  Field(new, 1) = list;
  CAMLreturn(new);
}

static value
event_list_of_int(int events)
{
  CAMLparam0();
  CAMLlocal1(result);

  result = Val_int(0);

  /* Do these in reverse order since we're prepending to the list */
  if (events & EPOLLHUP) {
    result = cons(Val_int(caml_POLLHUP), result);
  }
  if (events & EPOLLERR) {
    result = cons(Val_int(caml_POLLERR), result);
  }
  if (events & EPOLLOUT) {
    result = cons(Val_int(caml_POLLOUT), result);
  }
  if (events & EPOLLPRI) {
    result = cons(Val_int(caml_POLLPRI), result);
  }
  if (events & EPOLLIN) {
    result = cons(Val_int(caml_POLLIN), result);
  }
  CAMLreturn(result);
}

#ifdef EPOLL
/********************************************************************************
 *
 * epoll()
 *
 ********************************************************************************/

CAMLprim value
ocaml_epoll_create(value size)
{
  CAMLparam1(size);
  CAMLlocal1(result);

  int            ret;

  ret = epoll_create(Int_val(size));
  if (-1 == ret) {
    puke();
  }

  result = Val_int(ret);
  CAMLreturn(result);
}

CAMLprim value
ocaml_epoll_destroy(value t)
{
  CAMLparam1(t);

  int            ret;

  ret = close(Int_val(t));
  if (-1 == ret) {
    puke();
  }
  CAMLreturn(Val_unit);
}

/* There are three reasons why I can't store a continuation or any other
 * complex type in evt.data:
 *
 * 1. GC might blow them away
 * 2. Heap compaction might move them
 * 3. The kernel can remove events from its internal table without
 *    telling us (this is why there's no EPOLLNVAL)
 *
 * 1 and 2 can be solved by calling caml_register_global_root for each
 * continuation, but this does not solve 3.  So you get file
 * descriptors.  You can make a nice record type and wrap a Set around
 * it. */
CAMLprim value
ocaml_epoll_ctl(value t, value op, value what)
{
  CAMLparam3(t, op, what);

  int                 op_;
  int                 fd;
  struct epoll_event  evt;
  int                 ret;

  switch (Int_val(op)) {
    case caml_POLL_ADD:
      op_ = EPOLL_CTL_ADD;
      break;
    case caml_POLL_MOD:
      op_ = EPOLL_CTL_MOD;
      break;
    case caml_POLL_DEL:
      op_ = EPOLL_CTL_DEL;
      break;
  }
  fd = Int_val(Field(what, 0));
  evt.events = int_of_event_list(Field(what, 1));
  evt.data.fd = fd;

  ret = epoll_ctl(Int_val(t), op_, fd, &evt);
  if (-1 == ret) {
    puke();
  }

  CAMLreturn(Val_unit);
}

CAMLprim value
ocaml_epoll_wait(value t, value maxevents, value timeout)
{
  CAMLparam2(t, timeout);
  CAMLlocal2(result, item);

  int                maxevents_ = Int_val(maxevents);
  struct epoll_event events[maxevents_];
  int                i;
  int                ret;

  caml_enter_blocking_section();
  ret = epoll_wait(Int_val(t), events, maxevents_, Int_val(timeout));
  caml_leave_blocking_section();
  if (-1 == ret) {
    puke();
  }

  result = Val_int(0);
  for (i = 0; i < ret; i += 1) {
    item = alloc_small(2,0);
    Field(item, 0) = Val_int(events[i].data.fd);
    Field(item, 1) = event_list_of_int(events[i].events);
    result = cons(item, result);
  }

  CAMLreturn(result);
}

#else
/********************************************************************************
 *
 * poll() compatibility
 *
 ********************************************************************************/

struct t {
  int            nfds;
  int            size;
  struct pollfd *fds;
};

CAMLprim value
ocaml_epoll_create(value size)
{
  CAMLparam1(size);
  CAMLlocal1(result);

  struct t *t_;

  t_ = (struct t *)malloc(sizeof(struct t));
  t_->nfds = 0;
  t_->size = size;
  t_->fds = (struct pollfd *)calloc(size, sizeof(struct pollfd));

  result = caml_alloc(1, Abstract_tag);
  Field(result, 0) = (value)t_;

  CAMLreturn(result);
}

CAMLprim value
ocaml_epoll_destroy(value t)
{
  CAMLparam1(t);

  struct t *t_ = (struct t *)Field(t, 0);

  free(t_->fds);
  free(t_);
  CAMLreturn(Val_unit);
}


CAMLprim value
ocaml_epoll_ctl(value t, value op, value what)
{
  CAMLparam3(t, op, what);

  struct t *t_ = (struct t *)Field(t, 0);
  int            op_ = Int_val(op);
  struct pollfd  pfd;
  int            i;

  pfd.fd = Int_val(Field(what, 0));
  pfd.events = int_of_event_list(Field(what, 1));

  /* Find this fd in our list */
  for (i == 0; i < t_->nfds; i += 1) {
    struct pollfd *p = &(t_->fds[i]);

    if (p->fd == pfd.fd) {
      break;
    }
  }

  switch (op_) {
    case caml_POLL_ADD:
      if (i < t_->nfds) {
        caml_failwith("file descriptor already present");
      }
      if (i >= t_->size) {
        struct pollfd *newfds;
        int            newsize;

        newsize = t_->size + 20;
        newfds = (struct pollfd *)realloc(t_, (sizeof(struct pollfd)) * newsize);
        if (! newfds) {
          caml_failwith("out of memory");
        }
        t_->size = newsize;
        t_->fds = newfds;
      }
      t_->nfds += 1;
      t_->fds[i] = pfd;
      break;

    case caml_POLL_MOD:
      t_->fds[i] = pfd;
      break;

    case caml_POLL_DEL:
      if (i == t_->nfds) {
        caml_failwith("file descriptor not present");
      }
      t_->nfds -= 1;
      for(; i < t_->nfds; i += 1) {
        t_->fds[i] = t_->fds[i+1];
      }
      break;
  }
}

#include "obj.h"

CAMLprim value
ocaml_epoll_wait(value t, value maxevents, value timeout)
{
  CAMLparam3(t, maxevents, timeout);
  CAMLlocal2(result, v);

  struct t *t_         = (struct t *)Field(t, 0);
  int       maxevents_ = Int_val(maxevents);
  int       i;
  int       j;
  int       ret;

  /* Call poll */
  caml_enter_blocking_section();
  ret = poll(t_->fds, t_->nfds, Int_val(timeout));
  caml_leave_blocking_section();
  if (-1 == ret) {
    puke();
  }

  result = Val_int(0);
  if (0 < ret) {
    j = 0;
    for (i = 0; ((i < t_->nfds) && (i < maxevents_)); i += 1) {
      struct pollfd *p = &(t_->fds[i]);

      if (p->revents & POLLNVAL) {
        /* Don't let j increment: remove this item */
        continue;
      } else if (p->revents) {
        v = alloc_small(2, 0);
        Field(v, 0) = Val_int(p->fd);
        Field(v, 1) = event_list_of_int(p->revents);
        result = cons(v, result);
      }
      if (i != j) {
        t_->fds[i] = t_->fds[j];
      }
      j += 1;
    }
    t_->nfds = j;
  }
  CAMLreturn(result);
#error "I haven't yet figured out why this causes a segfault."
}

#endif
