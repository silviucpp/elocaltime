#ifndef C_SRC_ENIF_TIMEZONE_H_
#define C_SRC_ENIF_TIMEZONE_H_

#include "erl_nif.h"

void enif_timezone_free(ErlNifEnv* env, void* obj);

ERL_NIF_TERM enif_timezone_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM enif_timezone_civil_lookup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM enif_timezone_absolute_lookup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif
