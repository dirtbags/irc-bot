#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include <sysexit.h>

const char *x_is_y = "It's been said that %s is %s";
const char *added = "Okay, %s, I added a factoid to %s.";
const char *removed = "Okay, %s, I removed %d factoids from %s.";

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


/*
 *
 * CDB Interface
 *
 */

/* Why I am using stdio.h
 * By Neale Pickett
 * November, 2012
 *
 * I am not as clever as the people who maintain libc.
 *
 * THE END
 */

static uint32_t
hash(char *s, size_t len)
{
    uint32_t h = 5381;
    size_t i;

    for (i = 0; i < len; i += 1) {
        h = ((h << 5) + h) ^ s[i];
    }
    return h;
}


uint32_t
read_u32(FILE *f)
{
    uint8_t d[4];

    fread(d, 4, 1, f);
    return ((d[0]<< 0) |
            (d[1] << 8) |
            (d[2] << 16) |
            (d[3] << 24));
}


int
bufcmp(char *a, size_t alen, char *b, size_t blen)
{
    if (alen == blen) {
        return memcmp(a, b, blen);
    } else {
        return alen - blen;
    }
}

int
lookup(FILE *f, char *key)
{
    size_t keylen = strlen(key);
    uint32_t h = hash(key, keylen);
    uint32_t p, plen;
    uint32_t i;

    /* Read pointer */
    fseek(f, (h % 256) * 8, SEEK_SET);
    p = read_u32(f);
    plen = read_u32(f);

    /* Read hash table entries */
    for (i = (h / 256) % plen; i < plen; i += 1) {
        uint32_t hashval;
        uint32_t entry_pos;
        uint32_t klen;
        uint32_t dlen;

        fseek(f, p + (i * 8), SEEK_SET);
        hashval = read_u32(f);
        entry_pos = read_u32(f);
        if (entry_pos == 0) {
            break;
        }
        if (hashval != h) {
            continue;
        }

        fseek(f, entry_pos, SEEK_SET);
        klen = read_u32(f);
        dlen = read_u32(f);

        if (klen == keylen) {
            uint32_t i;

            for (i = 0; i < klen; i += 1) {
                int c = fgetc(f);

                if (c != key[i]) {
                    break;
                }
            }

            if (i < klen) {
                continue;
            }

            for (i = 0; i < dlen; i += 1) {
                int c = fgetc(f);

                putchar(c);
            }
        }
    }

    return 0;
}


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
    struct cdb c;
    FILE *f = fopen(filename, "r");
    size_t textlen = lowercase(text);
    uint32_t nresults;

    if (! f) {
        perror("Opening database");
        return EX_NOINPUT;
    }

    cdb_init(&c, f);

    /* Count how many results there are */
    cdb_lookup(&c, text, textlen);
    for (results = 0; cdb_next(&c, NULL, 0); results += 1);

    if (nresults > 0) {
        /* This is horrible: say rand() returned between 0 and 2, and results
         * was 2.  Possible values would be (0, 1, 0): not a uniform
         * distribution.  But this is random enough for our purposes. */
        uint32_t which = rand() % results;
        char val[8192];

        cdb_lookup(&c, text, textlen);
        for (results = 0; results < which; results += 1) {
            cdb_next(&c, NULL, 0);
        }
        cdb_next(&c, val, sizeof val);

        if (val[0] == '"') {
            printf("%s\n", val + 1);
        } else if (val[0] == ':') {
            printf("\001ACTION %s\001\n", val + 1);
        } else {
            printf(x_is_y, text, val);
        }
    }

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

    srand((unsigned int)time(NULL));

    filename = argv[1];
    text = argv[2];

    if ('!' == text[0]) {
        return infocmd(filename, text + 1);
    }
    return lookup(filename, text);
}
