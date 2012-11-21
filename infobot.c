#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/time.h>
#include <sysexits.h>
#include "cdb.h"
#include "cdbmake.h"

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

    return EX_USAGE;
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
choose(char *filename, char *key)
{
    struct cdb_ctx c;
    FILE *f = fopen(filename, "r");
    size_t keylen = lowercase(key);
    uint32_t nresults;

    if (! f) {
        perror("Opening database");
        return EX_NOINPUT;
    }

    cdb_init(&c, f);

    /* Count how many results there are */
    cdb_find(&c, key, keylen);
    for (nresults = 0; cdb_next(&c, NULL, 0); nresults += 1);

    if (nresults > 0) {
        /* This is horrible: say rand() returned between 0 and 2, and results
         * was 2.  Possible values would be (0, 1, 0): not a uniform
         * distribution.  But this is random enough for our purposes. */
        uint32_t which = rand() % nresults;
        uint32_t vallen;
        char val[8192];
        uint32_t i;

        cdb_find(&c, key, keylen);
        for (i = 0; i < which; i += 1) {
            cdb_next(&c, NULL, 0);
        }
        vallen = cdb_next(&c, val, sizeof(val));
        printf("%.*s\n", vallen, val);
    }

    fclose(f);

    return 0;
}

int
list(char *filename, char *key)
{
    struct cdb_ctx c;
    size_t keylen = lowercase(key);
    FILE *f = fopen(filename, "rb");

    if (! f) {
        perror("Opening database");
        return EX_NOINPUT;
    }

    cdb_init(&c, f);

    cdb_find(&c, key, keylen);
    for (;;) {
        uint32_t vallen;
        char val[8192];

        vallen = cdb_next(&c, val, sizeof(val)); 
        if (vallen == 0) {
            break;
        }
        printf("%.*s\n", vallen, val);
    }

    fclose(f);

    return 0;
}

int
add(char *filename, char *key, char *val)
{
    struct cdb_ctx inc;
    struct cdbmake_ctx outc;
    FILE *inf;
    FILE *outf;

    inf = fopen(filename, "rb");
    if (! inf) {
        perror("Opening database");
        return EX_NOINPUT;
    }

    {
        char fn[4096];

        snprintf(fn, sizeof(fn), "%s.%d", filename, getpid());
        outf = fopen(fn, "wb");
    }
    if (! outf) {
        perror("Creating temporary database");
        return EX_CANTCREAT;
    }

    cdb_init(&inc, inf);
    cdbmake_init(&outc, outf);

    for (;;) {
        char key[8192];
        char val[8192];
        size_t keylen = sizeof(key);
        size_t vallen = sizeof(val);

        if (EOF == cdb_dump(&inc, key, &keylen, val, &vallen)) {
            break;
        }
        cdbmake_add(&outc, key, keylen, val, vallen);
    }
    cdbmake_add(&outc, key, strlen(key), val, strlen(val));
    cdbmake_finalize(&outc);

    return 0;
}


enum action {
    ACT_ONE,
    ACT_ALL,
    ACT_ADD,
    ACT_DEL
};

int
main(int argc, char *argv[])
{
    char *filename;
    char *key;
    char *val;
    enum action act = ACT_ONE;

    for (;;) {
        int opt = getopt(argc, argv, "la:r:");

        if (-1 == opt) {
            break;
        }
        switch (opt) {
            case 'l':
                act = ACT_ALL;
                break;
            case 'a':
                act = ACT_ADD;
                val = optarg;
                break;
            case 'r':
                act = ACT_DEL;
                val = optarg;
                break;
            default:
                return usage(argv[0]);
        }
    }
    if (argc - optind != 2) {
        return usage(argv[0]);
    }

    // Seed PRNG with some crap
    {
        struct timeval tv;

        gettimeofday(&tv, NULL);
        srand((unsigned int)(tv.tv_sec * tv.tv_usec));
    }

    filename = argv[optind];
    key = argv[optind + 1];

    switch (act) {
        case ACT_ONE:
            return choose(filename, key);
        case ACT_ALL:
            return list(filename, key);
        case ACT_ADD:
            return add(filename, key, val);
        default:
            fprintf(stderr, "Not yet implemented, chump %s.\n", val);
            break;
    }

    return 0;
}
