#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <dlfcn.h>
#include "streebog/gost3411-2012-core.h"

#define assert(cond) \
  if (!(cond)) { return enif_make_badarg(env); } // XXX env expected

#define STREEBOG_CTX_ALIGN16(ptr) \
  ((GOST34112012Context *)(((uintptr_t)ptr+15) & ~ (uintptr_t)0x0F))
#define STREEBOG_CHECK_CTX(CTX) \
  (CTX->digest_size == 256 || CTX->digest_size == 512)

/*
static ERL_NIF_TERM init_streebog(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  void *resource;
  GOST34112012Context *CTX;
  int digest_size = 512;
  struct St *st = enif_priv_data(env);
  ERL_NIF_TERM ctx_term;
  assert(st);

  assert(resource = enif_alloc_resource(st->streebog_type, sizeof(GOST34112012Context)+15));
  assert(CTX = STREEBOG_CTX_ALIGN16(resource)); // XXX it's realy important

  if (argc > 0)
    assert(enif_get_int(env, argv[0], &digest_size) &&
          (digest_size == 512 || digest_size == 256));

  GOST34112012Init(CTX, digest_size);
  assert(STREEBOG_CHECK_CTX(CTX));
  
  ctx_term = enif_make_resource(env, resource);
  enif_release_resource(resource);
  return ctx_term;  
}

static ERL_NIF_TERM update_streebog(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  struct St *st = enif_priv_data(env);
  void *obj;
  GOST34112012Context *CTX;
  ErlNifBinary bin;
  assert(st);

  assert(enif_get_resource(env, argv[0], st->streebog_type, &obj));
  assert(CTX = STREEBOG_CTX_ALIGN16(obj));

  assert(enif_inspect_binary(env, argv[1], &bin));
  
  GOST34112012Update(CTX, bin.data, bin.size);
  
  return argv[0];
}

static ERL_NIF_TERM final_streebog(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  void *obj;
  GOST34112012Context *CTX;
  unsigned char *result;
  ERL_NIF_TERM bin;
  struct St *st = enif_priv_data(env);
  assert(st);

  assert(enif_get_resource(env, argv[0], st->streebog_type, &obj));
  assert(CTX = STREEBOG_CTX_ALIGN16(obj));
  assert(STREEBOG_CHECK_CTX(CTX));
  
  assert(result = enif_make_new_binary(env, CTX->digest_size / 8, &bin));
  GOST34112012Final(CTX, result);
  
  GOST34112012Cleanup(CTX); // … and cleaning CTX

  return bin;
}

void streebog_destroy(ErlNifEnv* caller_env, void* obj) {
  GOST34112012Context *CTX = STREEBOG_CTX_ALIGN16(obj);
  if (STREEBOG_CHECK_CTX(CTX))
    GOST34112012Cleanup(CTX);
}

*/