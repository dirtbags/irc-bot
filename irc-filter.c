#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <sysexits.h>

#include "dump.h"

#define MAX_ARGS 50
#define MAX_OUTARGS 60
#define MAX_PARTS 20

int
main(int argc, char *argv[])
{
  char *parts[20] = {0};
  int   nparts;
  char  snick[20];
  char *cmd;
  char *text      = NULL;
  char *prefix    = NULL;
  char *sender    = NULL;
  char *forum     = NULL;
  int   i;

  if (argc < 3) {
    fprintf(stderr, "Usage: %s HANDLER [ARGV ...] LINE\n", argv[0]);
    fprintf(stderr, "\n");
    fprintf(stderr, "Parses LINE (an IRC message) into:\n");
    fprintf(stderr, "   PREFIX           Prefix part of message\n");
    fprintf(stderr, "   COMMAND          IRC command\n");
    fprintf(stderr, "   SENDER           Nickname of message's sender\n");
    fprintf(stderr, "   FORUM            Forum of message\n");
    fprintf(stderr, "   TEXT             Text part of message\n");
    fprintf(stderr, "   ARGS...          Arguments of message\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "After parsing, exec()s\n");
    fprintf(stderr, "  HANDLER ARGV... PREFIX COMMAND SENDER FORUM TEXT ARGS...\n");
    return EX_USAGE;
  } else if (argc > MAX_ARGS) {
    fprintf(stderr, "%s: too many arguments\n", argv[0]);
    return EX_USAGE;
  }

  /* Tokenize IRC line */
  {
    char *line = argv[argc-1];

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
  }

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
    DUMP_d(nparts);
    if (1 == nparts) {
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
    char *_argv[MAX_OUTARGS + 1];

    _argc = 0;
    for (i = 1; i < argc-1; i += 1) {
      _argv[_argc++] = argv[i];
    }
    _argv[_argc++] = prefix?prefix:"";
    _argv[_argc++] = cmd;
    _argv[_argc++] = sender?sender:"";
    _argv[_argc++] = forum?forum:"";
    _argv[_argc++] = text?text:"";
    for (i = 1; (i < nparts) && (_argc < MAX_OUTARGS); i += 1) {
      _argv[_argc++] = parts[i];
    }
    _argv[_argc] = NULL;

    execvp(_argv[0], _argv);
    perror(_argv[0]);
  }

  return 0;
}
