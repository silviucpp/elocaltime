#ifndef C_SRC_ERLUAP_NIF_H_
#define C_SRC_ERLUAP_NIF_H_

#include "erl_nif.h"

struct atoms
{
    ERL_NIF_TERM atomOk;
    ERL_NIF_TERM atomUndefined;
    ERL_NIF_TERM atomTrue;
    ERL_NIF_TERM atomFalse;
    ERL_NIF_TERM atomError;
    ERL_NIF_TERM atomBadArg;

    ERL_NIF_TERM atomTimezoneUtc;
    ERL_NIF_TERM atomTimezoneLocal;
    ERL_NIF_TERM atomAbsLookup;
    ERL_NIF_TERM atomCivilLookup;
    ERL_NIF_TERM atomCivilKindUnique;
    ERL_NIF_TERM atomCivilKindSkipped;
    ERL_NIF_TERM atomCivilKindRepeated;
};

struct elocatime_data
{
    ErlNifResourceType* res_timezone;
};

extern atoms ATOMS;

#endif
