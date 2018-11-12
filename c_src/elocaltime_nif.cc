#include "elocaltime_nif.h"
#include "elocaltime.h"
#include "nif_utils.h"
#include "macros.h"

static const char kAtomOk[] = "ok";
static const char kAtomUndefined[] = "undefined";
static const char kAtomTrue[] = "true";
static const char kAtomFalse[] = "false";
static const char kAtomError[] = "error";
static const char kAtomBadArg[] = "badarg";
static const char kAtomTimezoneUtc[] = "tz_utc";
static const char kAtomTimezoneLocal[] = "tz_local";
static const char kAtomAbsLookup[] = "absolute_lookup";
static const char kAtomCivilLookup[] = "civil_lookup";
static const char kAtomCivilKindUnique[] = "unique";
static const char kAtomCivilKindSkipped[] = "skipped";
static const char kAtomCivilKindRepeated[] = "repeated";

atoms ATOMS;

void open_resources(ErlNifEnv* env, elocatime_data* data)
{
    ErlNifResourceFlags flags =  static_cast<ErlNifResourceFlags>(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    data->res_timezone = enif_open_resource_type(env, NULL, "res_timezone", enif_timezone_free, flags, NULL);
}

int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(load_info);

    ATOMS.atomOk = make_atom(env, kAtomOk);
    ATOMS.atomUndefined = make_atom(env, kAtomUndefined);
    ATOMS.atomTrue = make_atom(env, kAtomTrue);
    ATOMS.atomFalse = make_atom(env, kAtomFalse);
    ATOMS.atomError = make_atom(env, kAtomError);
    ATOMS.atomBadArg = make_atom(env, kAtomBadArg);
    ATOMS.atomTimezoneUtc = make_atom(env, kAtomTimezoneUtc);
    ATOMS.atomTimezoneLocal = make_atom(env, kAtomTimezoneLocal);
    ATOMS.atomAbsLookup = make_atom(env, kAtomAbsLookup);
    ATOMS.atomCivilLookup = make_atom(env, kAtomCivilLookup);
    ATOMS.atomCivilKindUnique = make_atom(env, kAtomCivilKindUnique);
    ATOMS.atomCivilKindSkipped = make_atom(env, kAtomCivilKindSkipped);
    ATOMS.atomCivilKindRepeated = make_atom(env, kAtomCivilKindRepeated);

    elocatime_data* data = static_cast<elocatime_data*>(enif_alloc(sizeof(elocatime_data)));
    open_resources(env, data);
    *priv_data = data;

    return 0;
}

int on_nif_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    UNUSED(old_priv);
    UNUSED(info);

    elocatime_data* data = static_cast<elocatime_data*>(enif_alloc(sizeof(elocatime_data)));
    open_resources(env, data);
    *priv = data;

    return 0;
}

void on_nif_unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);

    elocatime_data* data = static_cast<elocatime_data*>(priv_data);
    enif_free(data);
}

static ErlNifFunc nif_funcs[] = {
    {"new_timezone", 1, enif_timezone_new},
    {"absolute_lookup", 2, enif_timezone_absolute_lookup},
    {"civil_lookup", 2, enif_timezone_civil_lookup},
    {"format", 2, enif_format},
    {"format", 3, enif_format}
};

ERL_NIF_INIT(elocaltime_nif, nif_funcs, on_nif_load, NULL, on_nif_upgrade, on_nif_unload)
