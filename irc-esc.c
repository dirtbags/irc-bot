#include <stdio.h>

enum {
  RESET,
  COLOR,
  NOCOLOR,
  BOLD,
  UNDERLINE,
  INVERSE
};

int color_map[] = {7, 0, 4, 2, 1, 1, 5, 3, 3, 2, 6, 6, 4, 5, 0, 7};

void
change_state(int what, int foreground, int background)
{
  static int bf = 0;
  static int ul = 0;
  static int rv = 0;
  static int fg = -1;
  static int bg = -1;

  switch (what) {
    case RESET:
      fg = -1;
      bg = -1;
      bf = 0;
      ul = 0;
      rv = 0;
      break;
    case COLOR:
      fg = (foreground<16)?color_map[foreground]:-1;
      bg = (background<16)?color_map[background]:-1;
      break;
    case NOCOLOR:
      fg = -1;
      bg = -1;
      break;
    case BOLD:
      bf = !bf;
      break;
    case UNDERLINE:
      ul = !ul;
      break;
    case INVERSE:
      rv = !rv;
      break;
  }

  printf("\033[0");
  if (bf) printf(";1");
  if (ul) printf(";4");
  if (rv) printf(";7");
  if (0 <= fg) printf(";3%d", fg);
  if (0 <= bg) printf(";4%d", bg);
  printf("m");
}

int
read_num()
{
  int acc = 0;
  int fail = 1;

  while (1) {
    int c = getchar();

    if ((c >= '0') && (c <= '9')) {
      acc = (acc * 10) + (c - '0');
    } else {
      ungetc(c, stdin);
      break;
    }
    fail = 0;
  }
  return fail?-1:acc;
}

int
main(int argc, char *argv[])
{
  while (! feof(stdin)) {
    int c = getchar();

    if (EOF == c) {
      break;
    } else if (0 == c) {
      printf("\\0");
    } else if (3 == c) {  /* mIRC color */
      int fg = read_num();
      int bg = -1;
      
      c = getchar();
      if (',' == c) {
        bg = read_num();
      } else {
        ungetc(c, stdin);
      }
      change_state(COLOR, fg, bg);
    } else if (2 == c) {
      change_state(BOLD, 0, 0);
    } else if (22 == c) {
      change_state(INVERSE, 0, 0);
    } else if (31 == c) {
      change_state(UNDERLINE, 0, 0);
    } else if (15 == c) {
      change_state(RESET, 0, 0);
    } else if ('\n' == c) {
      change_state(RESET, 0, 0);
      putchar(c);
    } else if (32 > c) {
      printf("^%c", c + 'A' - 1);
    } else {
      putchar(c);
    }
  }
  change_state(RESET, 0, 0);
  return 0;
}
