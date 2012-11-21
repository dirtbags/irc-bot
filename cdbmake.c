#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>     // XXX: remove if malloc() is gone
#include "cdbmake.h"
#include "dump.h"

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

static void
write_u32le(FILE *f, uint32_t val)
{
    fputc((val >> 0)  & 0xff, f);
    fputc((val >> 8)  & 0xff, f);
    fputc((val >> 16) & 0xff, f);
    fputc((val >> 24) & 0xff, f);
}

void
cdbmake_init(struct cdbmake_ctx *ctx, FILE *f)
{
    int i;

    ctx->f = f;
    for (i = 0; i < 256; i += 1) {
        ctx->records[i] = NULL;
        ctx->nrecords[i] = 0;
    }
    
    ctx->where = 256 * 8;
    fseek(f, ctx->where, SEEK_SET);
}

void
cdbmake_add(struct cdbmake_ctx *ctx,
        char *key, size_t keylen,
        char *val, size_t vallen)
{
    uint32_t hashval = hash(key, keylen);
    int idx = hashval % 256;
    uint32_t n = ctx->nrecords[idx];

    ctx->nrecords[idx] += 1;
    ctx->records[idx] = (struct cdbmake_record *)realloc(ctx->records[idx],
            ctx->nrecords[idx] * sizeof(struct cdbmake_record));
    if (NULL == ctx->records[idx]) {
        perror("realloc records");
        return;
    }
    ctx->records[idx][n].hashval = hashval;
    ctx->records[idx][n].offset = (uint32_t)ctx->where;

    // Write it out
    write_u32le(ctx->f, keylen);
    write_u32le(ctx->f, vallen);
    fwrite(key, 1, keylen, ctx->f);
    fwrite(val, 1, vallen, ctx->f);

    ctx->where += 4 + 4 + keylen + vallen;
}

void
cdbmake_finalize(struct cdbmake_ctx *ctx)
{
    int idx;

    // Write out tables
    for (idx = 0; idx < 256; idx += 1) {
        uint32_t r;
        long offset;
        uint32_t tlen = ctx->nrecords[idx] * 2;
        uint32_t *buf;

        // Pointer
        offset = ftell(ctx->f);
        fseek(ctx->f, idx * 8, SEEK_SET);
        write_u32le(ctx->f, (uint32_t)offset);
        write_u32le(ctx->f, tlen);
        fseek(ctx->f, offset, SEEK_SET);

        // Build table in memory
        buf = (uint32_t *)calloc(tlen * 2, sizeof(uint32_t));
        if (! buf) {
            perror("Allocating hash table");
            return;
        }
        for (r = 0; r < ctx->nrecords[idx]; r += 1) {
            uint32_t slot = (ctx->records[idx][r].hashval / 256) % tlen;

            while (buf[slot * 2] > 0) {
                slot = (slot + 1) % tlen;
            }
            buf[slot*2 + 0] = ctx->records[idx][r].hashval;
            buf[slot*2 + 1] = ctx->records[idx][r].offset;
        }
        // Write it out
        for (r = 0; r < tlen; r += 1) {
            write_u32le(ctx->f, buf[r*2 + 0]);
            write_u32le(ctx->f, buf[r*2 + 1]);
        }
        free(buf);
    }

    ctx->f = NULL;
    
    for (idx = 0; idx < 256; idx += 1) {
        if (ctx->records[idx]) {
            free(ctx->records[idx]);
        }
        ctx->records[idx] = NULL;
        ctx->nrecords[idx] = 0;
    }
}
