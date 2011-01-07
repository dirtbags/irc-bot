#include <stdio.h>
#include <unistd.h>
#include <stdarg.h>
#include <sysexits.h>
#include <stdlib.h>
#include <signal.h>
#include <time.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/select.h>
#include <sys/time.h>

#include "dump.h"

#define MAX_ARGS 50
#define MAX_SUBPROCS 50
#define TARGET_MAX 20

#define max(a,b) ((a)>(b)?(a):(b))

struct subproc {
  int    fd;                    /* File descriptor */
  char   buf[4000];             /* Read buffer */
  size_t buflen;                /* Buffer length */
};

struct subproc subprocs[MAX_SUBPROCS] = {{0}};

/* Things set by argv parser */
char            *handler         = NULL;
char           **handler_args;
struct timeval   output_interval = {0};
struct timeval   output_last     = {0};
int              fifoin          = -1;
int              fifoout         = -1;

void
dispatch(const char *buf,
         size_t      buflen)
{
  int             subout[2];
  struct subproc *s = NULL;
  int             i;
  char            text[512];

  if (buflen > sizeof(text)) {
    fprintf(stderr, "Ignoring message: too long (%d bytes)\n", buflen);
    return;
  }
  memcpy(text, buf, buflen-1);  /* omit newline */
  text[buflen-1] = '\0';

  for (i = 0; i < MAX_SUBPROCS; i += 1) {
    if (0 == subprocs[i].fd) {
      s = &subprocs[i];
      break;
    }
  }
  if (! s) {
    fprintf(stderr, "Ignoring message: too many subprocesses\n");
    return;
  }

  if (-1 == pipe(subout)) {
    perror("pipe");
    return;
  }

  if (0 == fork()) {
    /* Child */
    char *argv[MAX_ARGS + 5];
    int   null;
    int   i;

    if ((-1 == (null = open("/dev/null", O_RDONLY))) ||
        (-1 == dup2(null, 0)) ||
        (-1 == dup2(subout[1], 1))) {
      perror("fd setup");
      exit(EX_OSERR);
    }

    /* We'll be good citizens about this and only close file descriptors
       we opened. */
    close(fifoout);
    close(null);
    close(subout[0]);
    close(subout[1]);
    for (i = 0; i < MAX_SUBPROCS; i += 1) {
      if (subprocs[i].fd) {
        close(subprocs[i].fd);
      }
    }

    i = 0;
    argv[i++] = handler;
    for (; handler_args[i-1]; i += 1) {
      argv[i] = handler_args[i-1];
    }
    argv[i++] = text;
    argv[i] = NULL;

    execvp(handler, argv);
    perror("exec");
    exit(0);
  }

  s->fd = subout[0];
  close(subout[1]);
}

void
delay_output()
{
  struct timeval now, diff;

  gettimeofday(&now, NULL);
  timersub(&now, &output_last, &diff);
  if (timercmp(&diff, &output_interval, <)) {
    struct timeval  delay;
    struct timespec ts;
    int             ret;

    timersub(&output_interval, &diff, &delay);

    ts.tv_sec = (time_t)delay.tv_sec;
    ts.tv_nsec = (long)(delay.tv_usec * 1000);
    do {
      ret = nanosleep(&ts, &ts);
    } while ((-1 == ret) && (EINTR == errno));
    gettimeofday(&output_last, NULL);
  } else {
    output_last = now;
  }
}


/** Writes all of buf to stdout, possibly blocking. */
void
output(const char *buf,
       size_t      count)
{
  if (timerisset(&output_interval)) {
    delay_output();
  }

  while (count) {
    ssize_t len;

    do {
      len = write(1, buf, count);
    } while ((-1 == len) && (EINTR == errno));
    if (-1 == len) {
      perror("stdout");
      exit(EX_IOERR);
    }
    count -= len;
    buf += len;
  }
}

void
call_with_lines(char   *buf,
                size_t *len,
                void (*func)(const char *, size_t))
{
  char   *b = buf;
  char   *p;
  size_t  l = *len;

  while ((p = memchr(b, '\n', l))) {
    size_t n = p - b + 1;
    size_t buflen = n;

    if ('\r' == *(p-1)) buflen -= 1;
    func(b, buflen);
    l -= n;
    b += n;
  }
  memmove(buf, b, l);
  *len = l;
}

char   inbuf[8000];
size_t inbuflen = 0;

void
handle_input()
{
  ssize_t len;

  do {
    len = read(0, inbuf + inbuflen, sizeof(inbuf) - inbuflen);
  } while ((-1 == len) && (EINTR == errno));
  if (0 == len) {
    exit(0);
  }
  inbuflen += len;
  call_with_lines(inbuf, &inbuflen, dispatch);
}

void
handle_subproc(struct subproc *s)
{
  ssize_t len;

  do {
    len = read(s->fd, s->buf + s->buflen, sizeof(s->buf) - s->buflen);
  } while ((-1 == len) && (EINTR == errno));
  if (-1 == len) {
    perror("subprocess read error");
  } else {
    s->buflen += len;
    call_with_lines(s->buf, &s->buflen, output);
  }

  if (sizeof(s->buf) == s->buflen) {
    fprintf(stderr, "subprocess buffer full, killing and discarding buffer.\n");
    len = 0;
  }

  /* Recycle this subproc unless something was read */
  if (0 >= len) {
    if (s->buflen) {
      fprintf(stderr, "warning: discarding %d characters from subprocess buffer\n",
              s->buflen);
    }
    close(s->fd);
    s->fd = 0;
    s->buflen = 0;
  }
}

void
loop()
{
  int    i, ret;
  int    nfds = 0;
  fd_set rfds;

  FD_ZERO(&rfds);
  FD_SET(0, &rfds);
  for (i = 0; i < MAX_SUBPROCS; i += 1) {
    if (subprocs[i].fd) {
      FD_SET(subprocs[i].fd, &rfds);
      nfds = max(nfds, subprocs[i].fd);
    }
  }

  do {
    ret = select(nfds+1, &rfds, NULL, NULL, NULL);
  } while ((-1 == ret) && (EINTR == errno));
  if (-1 == ret) {
    perror("select");
    exit(EX_IOERR);
  }

  if (FD_ISSET(0, &rfds)) {
    handle_input();
  }

  for (i = 0; i < MAX_SUBPROCS; i += 1) {
    if (subprocs[i].fd && FD_ISSET(subprocs[i].fd, &rfds)) {
      handle_subproc(&subprocs[i]);
    }
  }
}

void
sigchld(int signum)
{
  while (0 < waitpid(-1, NULL, WNOHANG));
}

void
usage(char *self)
{
  fprintf(stderr, "Usage: %s [OPTIONS] handler [ARGS ...]\n", self);
  fprintf(stderr, "\n");
  fprintf(stderr, "-f FIFO      Also dispatch messages from FIFO.\n");
  fprintf(stderr, "-i INTERVAL  Wait at least INTERVAL microseconds between\n");
  fprintf(stderr, "             sending each line.\n");
}

int
main(int argc, char *argv[])
{
  /* Parse command line */
  while (! handler) {
    switch (getopt(argc, argv, "hf:i:")) {
      case -1:
        if (optind >= argc) {
          fprintf(stderr, "error: must specify handler script.\n");
          usage(argv[0]);
          return EX_USAGE;
        }
        if (argc - optind - 10 > MAX_ARGS) {
          fprintf(stderr, "error: too many arguments to helper.\n");
          return EX_USAGE;
        }
        handler = argv[optind];
        handler_args = argv + (optind + 1);
        break;
      case 'f':
        if ((-1 == (fifoin = open(optarg, O_RDONLY | O_NONBLOCK))) ||
            (-1 == (fifoout = open(optarg, O_WRONLY)))) {
          perror("open fifo");
          return EX_IOERR;
        }
        subprocs[0].fd = fifoin;
        break;
      case 'i':
        {
          char          *end;
          long long int  interval;

          interval = strtoll(optarg, &end, 10);
          if (*end) {
            fprintf(stderr, "error: not an integer number: %s\n", optarg);
            return EX_USAGE;
          }
          output_interval.tv_sec = interval / 1000000;
          output_interval.tv_usec = interval % 1000000;
        }
        break;
      case 'h':
        usage(argv[0]);
        return 0;
      default:
        fprintf(stderr, "error: unknown option.\n");
        usage(argv[0]);
        return EX_USAGE;
    }
  }

  /* tcpclient uses fds 6 and 7.  If these aren't open, we keep the
     original fds 0 and 1. */
  if (-1 != dup2(6, 0)) close(6);
  if (-1 != dup2(7, 1)) close(7);

  signal(SIGCHLD, sigchld);

  while (1) {
    loop();
  }

  return 0;
}
