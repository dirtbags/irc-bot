#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#include <string.h>
#include <errno.h>
#include <poll.h>
#include <stdlib.h>

#define puke(str)                                                       \
  {                                                                     \
    char errstr[512];                                                   \
    snprintf(errstr, sizeof(errstr), str ": %s", strerror(errno));      \
    caml_failwith(errstr);                                              \
  }

value inspect_block(value v);

enum {
  caml_POLLIN,
  caml_POLLPRI,
  caml_POLLOUT,
  caml_POLLERR,
  caml_POLLHUP,
  caml_POLLNVAL
}

static int
list_length(value list)
{
  int   len;
  value l;

  for (l = list; l != Val_int(0); l = Field(l, 1)) {
    len += 1;
  }
  return len;
}

static int
int_of_event_list(value list)
{
  int   acc = 0;
  value l;

  for (l = list; l != Val_int(0); l = Field(l, 1)) {
    switch (Field(l, 0)) {
      case caml_POLLIN:
        acc |= POLLIN;
        break;
      case caml_POLLPRI:
        acc |= POLLPRI;
        break;
      case caml_POLLOUT:
        acc |= POLLOUT;
        break;
      case caml_POLLERR:
        acc |= POLLERR;
        break;
      case caml_POLLHUP:
        acc |= POLLHUP;
        break;
      case caml_POLLNVAL:
        acc |= POLLNVAL;
        break;
    }
  }
  return acc;
}

static value
append(value list, value item)
{
  value new = alloc_small(2, 0);
  Field(new, 0) = item;
  Field(new, 1) = list;
  return new;
}

static value
event_list_of_int(int events)
{
  value result = Val_int(0);

  if (events & POLLIN) {
    result = append(result, Val_int(caml_POLLIN));
  } else if (events & POLLPRI) {
    result = append(result, Val_int(caml_POLLPRI));
  } else if (events & POLLOUT) {
    result = append(result, Val_int(caml_POLLOUT));
  } else if (events & POLLERR) {
    result = append(result, Val_int(caml_POLLERR));
  } else if (events & POLLHUP) {
    result = append(result, Val_int(caml_POLLHUP));
  } else if (events & POLLNVAL) {
    result = append(result, Val_int(caml_POLLNVAL));
  }
  return result;
}

CAMLprim value
ocaml_poll(value caml_fds, value caml_timeout)
{
  CAMLparam2(caml_fds, caml_timeout);
  CAMLlocal1(result);

  value         l;
  int           nfds;
  int           timeout = Int_val(caml_timeout);
  struct pollfd fds[nfds];
  int           i;
  int           ret;

  result = Val_int(0);
  inspect_block(caml_fds);

  /* Count entries */
  nfds = list_length(caml_fds);

  /* Build fds */
  for (i=0, l = caml_fds; l != Val_int(0); i += 1, l = Field(l, 1)) {
    value f = Field(l, 0);
    struct pollfd *p = &(fds[i]);

    p->fd = Int_val(Field(f, 0));
    p->events = int_of_event_list(Field(f, 1));
  }

  /* Call poll */
  ret = poll(*fds, nfds, timeout);
  if (-1 == ret) {
    puke("poll");
  }

  for (i=0; i < nfds; i += 1) {
    struct pollfd *p = &(fds[i]);
    if (p->revents) {
      value v = alloc_small(2,0);

      Field(v, 0) = Val_int(p->fd);
      Field(v, 1) = event_list_of_int(p->revents);
      result = append(result, v);
    }
  }

  CAMLreturn(result);
}

