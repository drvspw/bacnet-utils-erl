#include "erl_nif.h"

#define OK "ok"

ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

static ERL_NIF_TERM
build_write_property_request_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  if(argc != 2) {
    return enif_make_badarg(env);
  }

  return mk_atom(env, OK);
}

static ERL_NIF_TERM
build_read_property_request_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  if(argc != 0) {
    return enif_make_badarg(env);
  }

  return mk_atom(env, OK);
}

static ErlNifFunc nif_funcs[] = {
  {"build_write_property_request_nif", 2, build_write_property_request_nif},
  {"build_read_property_request_nif", 0, build_read_property_request_nif}
};

ERL_NIF_INIT(bacnet_utils, nif_funcs, NULL, NULL, NULL, NULL);
