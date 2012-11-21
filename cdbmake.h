#ifndef __CDBMAKE_H__
#define __CDBMAKE_H__

#include <stdio.h>
#include <stdint.h>
#include <string.h>

struct cdbmake_record {
    uint32_t hashval;
    uint32_t offset;
};

struct cdbmake_ctx {
    FILE *f;
    struct cdbmake_record *records[256];
    uint32_t nrecords[256];
};

void cdbmake_init(struct cdbmake_ctx *ctx, FILE *f);
void cdbmake_add(struct cdbmake_ctx *ctx,
        char *key, size_t keylen,
        char *val, size_t vallen);
void cdbmake_finalize(struct cdbmake_ctx *ctx);

#endif
