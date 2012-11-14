#include <stdio.h>
#include <string.h>
#include <stdint.h>

/* Why I am using stdio.h
 * By Neale Pickett
 * November, 2012
 *
 * I am not as clever as the people who maintain libc.
 *
 * THE END
 */

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


uint32_t
hash(char *s, size_t len)
{
    uint32_t h = 5381;
    size_t i;

    for (i = 0; i < len; i += 1) {
        h = ((h << 5) + h) ^ s[i];
    }
    return h;
}

int
usage()
{
    fprintf(stderr, "Usage: infobot\n");

    return 0;
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
main(int argc, char *argv[])
{
    if (1 == argc) {
        return usage();
    }

    return lookup(stdin, argv[1]);
}
