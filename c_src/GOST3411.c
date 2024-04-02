#include <sys/types.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <dlfcn.h>
#include "streebog/gost3411-2012-core.h"

#define assert(cond) \
  if (!(cond)) { printf("assert!\n"); return NULL; }

#define STREEBOG_CTX_ALIGN16(ptr) \
  ((GOST34112012Context *)(((uintptr_t)ptr+15) & ~ (uintptr_t)0x0F))
#define STREEBOG_CHECK_CTX(CTX) \
  (CTX->digest_size == 256 || CTX->digest_size == 512)


int ctx_size() { return sizeof (struct GOST34112012Context); }

int ctx_offset_buffer() { return offsetof(struct GOST34112012Context, buffer); }
int ctx_offset_hash() { return offsetof(struct GOST34112012Context, hash); }
int ctx_offset_h() { return offsetof(struct GOST34112012Context, h); }
int ctx_offset_N() { return offsetof(struct GOST34112012Context, N); }
int ctx_offset_Sigma() { return offsetof(struct GOST34112012Context, Sigma); }
int ctx_offset_bufsize() { return offsetof(struct GOST34112012Context, bufsize); }
int ctx_offset_digest_size() { return offsetof(struct GOST34112012Context, digest_size); }

