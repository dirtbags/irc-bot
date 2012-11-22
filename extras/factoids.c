#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <fnmatch.h>
#include <ctype.h>
#include <sys/time.h>
#include <sysexits.h>
#include "cdb.h"
#include "cdbmake.h"

int
usage(char *self)
{
    fprintf(stderr, "Usage: %s [OPTIONS] CDB \"KEY\"\n", self);
    fprintf(stderr, "\n");
    fprintf(stderr, "Default:   Display one randomly-picked entry for KEY\n");
    fprintf(stderr, "-n         Create database from scratch, ignoring KEY\n");
    fprintf(stderr, "-l         Display all entries for KEY\n");
    fprintf(stderr, "-a VAL     Append VAL to entries for KEY\n");
    fprintf(stderr, "-r GLOB    Remove entries matching GLOB from KEY\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "KEY is always converted to lowercase (Latin-1 only)\n");

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

static int
setup_copy(char *infn, struct cdb_ctx *inc, FILE **inf,
        char **outfn, struct cdbmake_ctx *outc, FILE **outf)
{
    static char tmpfn[8192];

    *inf = fopen(infn, "rb");
    if (! *inf) {
        perror("Opening database");
        return EX_NOINPUT;
    }

    snprintf(tmpfn, sizeof(tmpfn), "%s.%d", infn, getpid());
    *outf = fopen(tmpfn, "wb");
    if (! *outf) {
        perror("Creating temporary database");
        return EX_CANTCREAT;
    }

    cdb_init(inc, *inf);
    cdbmake_init(outc, *outf);

    *outfn = strdup(tmpfn);

    return 0;
}

static void
finish_copy(char *infn, struct cdb_ctx *inc, FILE **inf,
        char *outfn, struct cdbmake_ctx *outc, FILE **outf)
{
    cdbmake_finalize(outc);
    fclose(*outf);
    fclose(*inf);

    rename(outfn, infn);
    free(outfn);
}

int
add(char *filename, char *key, char *val)
{
    struct cdb_ctx inc;
    struct cdbmake_ctx outc;
    FILE *inf;
    FILE *outf;
    char *outfn;
    int ret;

    ret = setup_copy(filename, &inc, &inf, &outfn, &outc, &outf);
    if (ret) {
        return ret;
    }

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

    finish_copy(filename, &inc, &inf, outfn, &outc, &outf);

    return 0;
}

int
del(char *filename, char *key, char *glob)
{
    size_t keylen = strlen(key);
    struct cdb_ctx inc;
    struct cdbmake_ctx outc;
    FILE *inf;
    FILE *outf;
    char *outfn;
    int ret;

    ret = setup_copy(filename, &inc, &inf, &outfn, &outc, &outf);
    if (ret) {
        return ret;
    }

    for (;;) {
        char k[8192];
        char v[8192];
        size_t klen = sizeof(k);
        size_t vlen = sizeof(v) - 1;

        if (EOF == cdb_dump(&inc, k, &klen, v, &vlen)) {
            break;
        }

        v[vlen] = '\0';
        if ((klen == keylen) &&
                (0 == memcmp(k, key, klen)) &&
                (0 == fnmatch(glob, v, 0))) {
            // Skip if it matches
        } else {
            cdbmake_add(&outc, k, klen, v, vlen);
        }
    }

    finish_copy(filename, &inc, &inf, outfn, &outc, &outf);
    return 0;
}

int
create(char *filename)
{
    FILE *f = fopen(filename, "wb");
    struct cdbmake_ctx outc;

    if (! f) {
        perror("Creating database");
        return EX_CANTCREAT;
    }

    cdbmake_init(&outc, f);
    cdbmake_finalize(&outc);

    return 0;
}

enum action {
    ACT_ONE,
    ACT_ALL,
    ACT_ADD,
    ACT_DEL,
    ACT_NEW
};

int
main(int argc, char *argv[])
{
    char *filename;
    char *key;
    char *val;
    enum action act = ACT_ONE;

    for (;;) {
        int opt = getopt(argc, argv, "hlna:r:");

        if (-1 == opt) {
            break;
        }
        switch (opt) {
            case 'l':
                act = ACT_ALL;
                break;
            case 'n':
                act = ACT_NEW;
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

    if (! (filename = argv[optind++])) {
        return usage(argv[0]);
    }
    if ((act != ACT_NEW) &&
            (! (key = argv[optind++]))) {
        return usage(argv[0]);
    }
    if (argv[optind]) {
        return usage(argv[0]);
    }

    // Seed PRNG with some crap
    {
        struct timeval tv;

        gettimeofday(&tv, NULL);
        srand((unsigned int)(tv.tv_sec * tv.tv_usec));
    }

    switch (act) {
        case ACT_ONE:
            return choose(filename, key);
        case ACT_ALL:
            return list(filename, key);
        case ACT_ADD:
            return add(filename, key, val);
        case ACT_DEL:
            return del(filename, key, val);
        case ACT_NEW:
            return create(filename);
    }

    return 0;
}
