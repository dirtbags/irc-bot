#ifndef __CDB_H__
#define __CDB_H__

#include <stdio.h>
#include <stdint.h>

struct cdb_ctx {
    FILE *f;

    char *key;
    uint32_t keylen;

    uint32_t hash_val;
    uint32_t hash_pos;
    uint32_t hash_len;

    uint32_t entry;
};

void cdb_init(struct cdb_ctx *ctx, FILE *f);
void cdb_find(struct cdb_ctx *ctx, char *key, uint32_t keylen);
uint32_t cdb_next(struct cdb_ctx *ctx, char *buf, uint32_t buflen);

#endif
