#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include <errno.h>
#include <sysexits.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <fcntl.h>
#include "dispatch.h"

#define MAX_ARGS 50
#define MAX_SUBPROCS 50
#define TARGET_MAX 20

#define max(a,b) ((a)>(b)?(a):(b))

char *handler = NULL;
char *msgdir = NULL;
struct timeval output_interval = {0};

void
maybe_setenv(char *key, char *val)
{
    if (val) {
        setenv(key, val, 1);
    }
}

void
irc_filter(char *line)
{
    char *parts[20] = {0};
    int   nparts;
    char  snick[20];
    char *cmd;
    char *text = NULL;
    char *prefix = NULL;
    char *sender = NULL;
    char *forum = NULL;
    int   i;

    /* Tokenize IRC line */
    nparts = 0;
    if (':' == *line) {
        prefix = line + 1;
    } else {
        parts[nparts++] = line;
    }
    while (*line) {
        if (' ' == *line) {
            *line++ = '\0';
            if (':' == *line) {
                text = line+1;
                break;
            } else {
                parts[nparts++] = line;
            }
        } else {
            line += 1;
        }
    }

    /* Strip trailing carriage return */
    while (*line) line += 1;
    if ('\r' == *(line-1)) *(line-1) = '\0';

    /* Set command, converting to upper case */
    cmd = parts[0];
    for (i = 0; cmd[i]; i += 1) {
        cmd[i] = toupper(cmd[i]);
    }

    /* Extract prefix nickname */
    for (i = 0; prefix && (prefix[i] != '!'); i += 1) {
        if (i == sizeof(snick) - 1) {
            i = 0;
            break;
        }
        snick[i] = prefix[i];
    }
    snick[i] = '\0';
    if (i) {
        sender = snick;
    }

    /* Determine forum */
    if ((0 == strcmp(cmd, "PRIVMSG")) ||
            (0 == strcmp(cmd, "NOTICE"))) {
        /* :neale!user@127.0.0.1 PRIVMSG #hydra :foo */
        switch (parts[1][0]) {
            case '#':
            case '&':
            case '+': 
            case '!':
                forum = parts[1];
                break;
            default:
                forum = snick;
                break;
        }
    } else if ((0 == strcmp(cmd, "PART")) ||
            (0 == strcmp(cmd, "MODE")) ||
            (0 == strcmp(cmd, "TOPIC")) ||
            (0 == strcmp(cmd, "KICK"))) {
        forum = parts[1];
    } else if (0 == strcmp(cmd, "JOIN")) {
        if (0 == nparts) {
            forum = text;
            text = NULL;
        } else {
            forum = parts[1];
        }
    } else if (0 == strcmp(cmd, "INVITE")) {
        forum = text?text:parts[2];
        text = NULL;
    } else if (0 == strcmp(cmd, "NICK")) {
        sender = parts[1];
        forum = sender;
    } else if (0 == strcmp(cmd, "PING")) {
        printf("PONG :%s\r\n", text);
        fflush(stdout);
    }

    {
        int   _argc;
        char *_argv[MAX_ARGS + 1];

        maybe_setenv("handler", handler);
        maybe_setenv("prefix", prefix);
        maybe_setenv("command", cmd);
        maybe_setenv("sender", sender);
        maybe_setenv("forum", forum);
        maybe_setenv("text", text);

        _argc = 0;
        _argv[_argc++] = handler;
        for (i = 1; (i < nparts) && (_argc < MAX_ARGS); i += 1) {
            _argv[_argc++] = parts[i];
        }
        _argv[_argc] = NULL;

        execvp(handler, _argv);
        perror(handler);
    }
}

struct subproc {
    int fd;                     /* File descriptor */
    char buf[4000];             /* Read buffer */
    size_t buflen;              /* Buffer length */
};

struct subproc subprocs[MAX_SUBPROCS] = { {0} };

void
dispatch(const char *buf, size_t buflen)
{
    int subout[2];
    struct subproc *s = NULL;
    int i;
    char text[512];

    if (buflen > sizeof(text)) {
        fprintf(stderr, "Ignoring message: too long (%u bytes)\n", (unsigned int) buflen);
        return;
    }
    memcpy(text, buf, buflen - 1);      /* omit newline */
    text[buflen - 1] = '\0';

    for (i = 0; i < MAX_SUBPROCS; i += 1) {
        if (0 == subprocs[i].fd) {
            s = &subprocs[i];
            break;
        }
    }
    if (!s) {
        fprintf(stderr, "Ignoring message: too many subprocesses\n");
        return;
    }

    if (-1 == pipe(subout)) {
        perror("pipe");
        return;
    }

    if (0 == fork()) {
        /*
         * Child 
         */
        int null;

        if ((-1 == (null = open("/dev/null", O_RDONLY))) ||
                (-1 == dup2(null, 0)) ||
                (-1 == dup2(subout[1], 1))) {
            perror("fd setup");
            exit(EX_OSERR);
        }

        /*
         * We'll be a good citizen and only close file descriptors we opened. 
         */
        close(null);
        close(subout[0]);
        close(subout[1]);
        for (i = 0; i < MAX_SUBPROCS; i += 1) {
            if (subprocs[i].fd) {
                close(subprocs[i].fd);
            }
        }

        irc_filter(text);
        exit(0);
    }

    s->fd = subout[0];
    close(subout[1]);
}

void
delay_output()
{
    struct timeval now;
    struct timeval diff;
    static struct timeval output_last = { 0 };

    gettimeofday(&now, NULL);
    timersub(&now, &output_last, &diff);
    if (timercmp(&diff, &output_interval, <)) {
        struct timeval delay;
        struct timespec ts;
        int ret;

        timersub(&output_interval, &diff, &delay);

        ts.tv_sec = (time_t) delay.tv_sec;
        ts.tv_nsec = (long) (delay.tv_usec * 1000);
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
output(const char *buf, size_t count)
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
call_with_lines(char *buf, size_t * len, void (*func) (const char *, size_t))
{
    char *b = buf;
    char *p;
    size_t l = *len;

    while ((p = memchr(b, '\n', l))) {
        size_t n = p - b + 1;

        func(b, n);
        l -= n;
        b += n;
    }
    memmove(buf, b, l);
    *len = l;
}

char inbuf[8000];
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

    /*
     * Recycle this subproc unless something was read 
     */
    if (0 >= len) {
        if (s->buflen) {
            fprintf(stderr, "warning: discarding %u characters from subprocess buffer\n", (unsigned int) s->buflen);
        }
        close(s->fd);
        s->fd = 0;
        s->buflen = 0;
    }
}

void
loop()
{
    int i;
    int ret;
    int nfds = 0;
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
        ret = select(nfds + 1, &rfds, NULL, NULL, NULL);
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
    fprintf(stderr, "Usage: %s [OPTIONS] HANDLER\n", self);
    fprintf(stderr, "\n");
    fprintf(stderr, "-h           Display help.\n");
    fprintf(stderr, "-d DIR       Also dispatch messages from DIR, one per file.\n");
    fprintf(stderr, "-i INTERVAL  Wait at least INTERVAL microseconds between\n");
    fprintf(stderr, "             sending each line.\n");
}

int
main(int argc, char *argv[])
{
    /*
     * Parse command line 
     */
    while (!handler) {
        switch (getopt(argc, argv, "hd:i:")) {
            case -1:
                if (optind >= argc) {
                    fprintf(stderr, "error: must specify event handler.\n");
                    usage(argv[0]);
                    return EX_USAGE;
                }
                handler = argv[optind];
                break;
            case 'd':
                msgdir = optarg;
                break;
            case 'i':
                {
                    char *end;
                    long long int interval;

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

    /*
     * tcpclient uses fds 6 and 7.  If these aren't open, we keep the
     * original fds 0 and 1. 
     */
    if (-1 != dup2(6, 0)) {
        close(6);
    }
    if (-1 != dup2(7, 1)) {
        close(7);
    }

    signal(SIGCHLD, sigchld);

    while (1) {
        loop();
    }

    return 0;
}
