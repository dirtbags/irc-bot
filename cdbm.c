#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>     // XXX: remove if malloc() is gone

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

void
write_u32le(FILE *f, uint32_t val)
{
    fputc((val >> 0)  & 0xff, f);
    fputc((val >> 8)  & 0xff, f);
    fputc((val >> 16) & 0xff, f);
    fputc((val >> 24) & 0xff, f);
}

struct record {
    uint32_t hashval;
    uint32_t offset;
};

int
main(int argc, char *argv[])
{
    struct record *records[256] = {0};
    uint32_t nrecords[256] = {0};
    FILE *f = fopen("a.cdb", "wb");
    int idx;

    // Start writing records
    fseek(f, 256*4*2, SEEK_SET);

    while (! feof(stdin)) {
        int ret;
        char key[4098];
        char val[4098];
        uint32_t keylen;
        uint32_t vallen;

        // Read a record
        ret = scanf("+%u,%u:", &keylen, &vallen);
        if (ret == 0) {
            break;
        }
        if ((keylen > sizeof(key)) || (vallen > sizeof(val))) {
            fprintf(stderr, "error: my buffers are too puny (%d,%d)\n", keylen, vallen);
            return 1;
        }
        fread(key, 1, keylen, stdin);
        getchar();
        getchar();
        fread(val, 1, vallen, stdin);
        getchar();

        // Comupute hash of key
        {
            long where = ftell(f);
            uint32_t hashval = hash(key, keylen);
            uint32_t n;

            idx = hashval % 256;
            n = nrecords[idx];
            nrecords[idx] += 1;
            records[idx] = (struct record *)realloc(records[idx], nrecords[idx] * sizeof(struct record));
            if (NULL == records[idx]) {
                perror("realloc records");
                return 1;
            }

            records[idx][n].hashval = hashval;
            records[idx][n].offset = (uint32_t)where;
        }

        // Write it out
        write_u32le(f, keylen);
        write_u32le(f, vallen);
        fwrite(key, 1, keylen, f);
        fwrite(val, 1, vallen, f);
    }

    // Write out tables
    for (idx = 0; idx < 256; idx += 1) {
        uint32_t r;
        long offset;
        uint32_t tlen = nrecords[idx] * 2;
        uint32_t *buf;

        // Pointer
        offset = ftell(f);
        fseek(f, idx * 8, SEEK_SET);
        write_u32le(f, (uint32_t)offset);
        write_u32le(f, tlen);
        fseek(f, offset, SEEK_SET);

        // Build table in memory
        buf = (uint32_t *)calloc(tlen * 2, sizeof(uint32_t));
        if (! buf) {
            perror("Allocating hash table");
            return 1;
        }
        for (r = 0; r < nrecords[idx]; r += 1) {
            uint32_t slot = (records[idx][r].hashval / 256) % tlen;

            while (buf[slot * 2] > 0) {
                slot = (slot + 1) % tlen;
            }
            buf[slot*2 + 0] = records[idx][r].hashval;
            buf[slot*2 + 1] = records[idx][r].offset;
        }
        // Write it out
        for (r = 0; r < tlen; r += 1) {
            write_u32le(f, buf[r*2 + 0]);
            write_u32le(f, buf[r*2 + 1]);
        }
        free(buf);
    }

    fclose(f);

    return 0;
}
