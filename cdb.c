#include <stdio.h>
#include <string.h>
#include <stdint.h>
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

#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif

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

void
cdb_init(struct cdb_ctx *ctx, FILE *f)
{
    ctx->f = f;
    ctx->key = NULL;
}

void
cdb_find(struct cdb_ctx *ctx, char *key, uint32_t keylen)
{
    ctx->hash_val = hash(key, keylen);

    ctx->key = key;
    ctx->keylen = keylen;

    /* Read pointer */
    fseek(ctx->f, (ctx->hash_val % 256) * 8, SEEK_SET);
    ctx->hash_pos = read_u32(ctx->f);
    ctx->hash_len = read_u32(ctx->f);
    ctx->entry = (ctx->hash_val / 256) % ctx->hash_len;
}

uint32_t
cdb_next(struct cdb_ctx *ctx, char *buf, uint32_t buflen)
{
    uint32_t hashval;
    uint32_t entry_pos;
    uint32_t klen;
    uint32_t dlen;

    for (;;) {
        fseek(ctx->f, ctx->hash_pos + (ctx->entry++ * 8), SEEK_SET);
        hashval = read_u32(ctx->f);
        entry_pos = read_u32(ctx->f);
        if (entry_pos == 0) {
            break;
        }
        if (hashval != ctx->hash_val) {
            continue;
        }

        fseek(ctx->f, entry_pos, SEEK_SET);
        klen = read_u32(ctx->f);
        dlen = read_u32(ctx->f);

        if (klen == ctx->keylen) {
            uint32_t i;

            for (i = 0; i < klen; i += 1) {
                int c = fgetc(ctx->f);

                if (c != ctx->key[i]) {
                    break;
                }
            }

            if (i < klen) {
                continue;
            }

            return fread(buf, 1, min(dlen, buflen), ctx->f);
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

    {
        struct cdb_ctx ctx;
        char buf[8192];
        int32_t r;

        cdb_init(&ctx, stdin);
        cdb_find(&ctx, argv[1], strlen(argv[1]));
        while ((r = cdb_next(&ctx, buf, sizeof buf))) {
            printf("%.*s\n", r, buf);
        }
    }

    return 0;
}
