#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include "cdb.h"

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

#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif

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

static uint32_t
read_u32le(FILE *f)
{
    uint8_t d[4];

    fread(d, 4, 1, f);
    return ((d[0]<< 0) |
            (d[1] << 8) |
            (d[2] << 16) |
            (d[3] << 24));
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
    ctx->hash_pos = read_u32le(ctx->f);
    ctx->hash_len = read_u32le(ctx->f);
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
        fseek(ctx->f, ctx->hash_pos + (ctx->entry * 8), SEEK_SET);
        ctx->entry = (ctx->entry + 1) % ctx->hash_len;

        hashval = read_u32le(ctx->f);
        entry_pos = read_u32le(ctx->f);
        if (entry_pos == 0) {
            break;
        }
        if (hashval != ctx->hash_val) {
            continue;
        }

        fseek(ctx->f, entry_pos, SEEK_SET);
        klen = read_u32le(ctx->f);
        dlen = read_u32le(ctx->f);

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

            if (buf) {
                return fread(buf, 1, min(dlen, buflen), ctx->f);
            } else {
                return dlen;
            }
        }
    }

    return 0;
}

