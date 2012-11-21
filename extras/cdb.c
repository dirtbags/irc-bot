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

    if (0 == fread(d, 4, 1, f)) {
        return 0;
    }
    return ((d[0] << 0) |
            (d[1] << 8) |
            (d[2] << 16) |
            (d[3] << 24));
}

static uint32_t
read_buf(FILE *f, uint32_t fieldlen, char *buf, size_t buflen)
{

    uint32_t size = min(buflen, fieldlen);
    size_t r = fread(buf, 1, (size_t)size, f);

    // Slurp up the rest
    for (fieldlen -= r; fieldlen > 0; fieldlen -= 1) {
        getc(f);
    }

    return size;
}

void
cdb_init(struct cdb_ctx *ctx, FILE *f)
{
    ctx->f = f;
    ctx->key = NULL;
    ctx->hash_len = 1;
}

int
cdb_dump(struct cdb_ctx *ctx,
        char *key, size_t *keylen,
        char *val, size_t *vallen)
{
    // Set hash_len to 0 to signal we're in position
    if (ctx->hash_len != 0) {
        // Find out where to stop reading
        int i;

        fseek(ctx->f, 0, SEEK_SET);
        ctx->hash_len = 0;
        ctx->hash_pos = 0xffffffff;
        for (i = 0; i < 256; i += 1) {
            uint32_t p;

            p = read_u32le(ctx->f);
            read_u32le(ctx->f);
            if (p < ctx->hash_pos) {
                ctx->hash_pos = p;
            }
        }

        ctx->entry = 256 * 8;
        fseek(ctx->f, ctx->entry, SEEK_SET);
    } 
    
    // Stop if we've reached the end
    if (ctx->entry >= ctx->hash_pos) {
        return EOF;
    }

    // Read the two buffers
    {
       uint32_t klen = read_u32le(ctx->f);
       uint32_t vlen = read_u32le(ctx->f);

       *keylen = read_buf(ctx->f, klen, key, *keylen);
       *vallen = read_buf(ctx->f, vlen, val, *vallen);

        ctx->entry += 4 + 4 + klen + vlen;
    }

    return 0;
}

void
cdb_find(struct cdb_ctx *ctx, char *key, size_t keylen)
{
    ctx->key = key;
    ctx->keylen = keylen;

    ctx->hash_val = hash(key, keylen);

    // Read pointer
    fseek(ctx->f, (ctx->hash_val % 256) * 8, SEEK_SET);
    ctx->hash_pos = read_u32le(ctx->f);
    ctx->hash_len = read_u32le(ctx->f);
    ctx->entry = (ctx->hash_val / 256) % ctx->hash_len;
}

uint32_t
cdb_next(struct cdb_ctx *ctx, char *buf, size_t buflen)
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
                return read_buf(ctx->f, dlen, buf, buflen);
            } else {
                return dlen;
            }
        }
    }

    return 0;
}

