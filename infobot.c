#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/time.h>
#include <sysexits.h>
#include "cdb.h"

/* Some things I use for debugging */
#ifdef NODUMP
#  define DUMPf(fmt, args...)
#else
#  define DUMPf(fmt, args...) fprintf(stderr, "%s:%s:%d " fmt "\n", __FILE__, __FUNCTION__, __LINE__, ##args)
#endif
#define DUMP() DUMPf("")
#define DUMP_u(v) DUMPf("%s = %u", #v, v)
#define DUMP_d(v) DUMPf("%s = %d", #v, v)
#define DUMP_x(v) DUMPf("%s = 0x%x", #v, v)
#define DUMP_s(v) DUMPf("%s = %s", #v, v)
#define DUMP_c(v) DUMPf("%s = %c", #v, v)
#define DUMP_p(v) DUMPf("%s = %p", #v, v)

int
usage()
{
    fprintf(stderr, "Usage: infobot factoids.cdb \"text\"\n");

    return 0;
}

size_t
lowercase(char *text)
{
    size_t ret;

    for (ret = 0; text[ret]; ret += 1) {
        text[ret] = tolower(text[ret]);
    }

    return ret;
}

int
infocmd(char *filename, char *text)
{
    return 0;
}

int 
lookup(char *filename, char *text)
{
    struct cdb_ctx c;
    FILE *f = fopen(filename, "r");
    size_t textlen = lowercase(text);
    uint32_t nresults;

    if (! f) {
        perror("Opening database");
        return EX_NOINPUT;
    }

    cdb_init(&c, f);

    /* Count how many results there are */
    cdb_find(&c, text, textlen);
    for (nresults = 0; cdb_next(&c, NULL, 0); nresults += 1);

    if (nresults > 0) {
        /* This is horrible: say rand() returned between 0 and 2, and results
         * was 2.  Possible values would be (0, 1, 0): not a uniform
         * distribution.  But this is random enough for our purposes. */
        uint32_t which = rand() % nresults;
        uint32_t vallen;
        char val[8192];
        uint32_t i;

        cdb_find(&c, text, textlen);
        for (i = 0; i < which; i += 1) {
            cdb_next(&c, NULL, 0);
        }
        vallen = cdb_next(&c, val, sizeof val);
        printf("%.*s\n", vallen, val);
    }

    fclose(f);

    return 0;
}

int
main(int argc, char *argv[])
{
    char *filename;
    char *text;

    if (3 != argc) {
        return usage();
    }

    {
        struct timeval tv;

        gettimeofday(&tv, NULL);
        srand((unsigned int)(tv.tv_sec * tv.tv_usec));
    }

    filename = argv[1];
    text = argv[2];

    return lookup(filename, text);
}
