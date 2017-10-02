#include "enif_timezone.h"
#include "elocaltime_nif.h"
#include "macros.h"
#include "nif_utils.h"
#include "cctz/time_zone.h"

#include <chrono>

namespace {

const char kUnknownTimezone[] = "unknown timezone";

std::chrono::system_clock::time_point timestamp2tp(uint64_t ts)
{
    std::chrono::duration<uint64_t> sec(ts);
    return std::chrono::system_clock::time_point(std::chrono::duration_cast<std::chrono::system_clock::duration>(sec));
}

uint64_t tp2timestamp(const std::chrono::system_clock::time_point& tp)
{
    return std::chrono::duration_cast<std::chrono::seconds>(tp.time_since_epoch()).count();
}

ERL_NIF_TERM kind2atom(cctz::time_zone::civil_lookup::civil_kind kind)
{
    switch (kind)
    {
        case cctz::time_zone::civil_lookup::UNIQUE:
            return ATOMS.atomCivilKindUnique;

        case cctz::time_zone::civil_lookup::REPEATED:
            return ATOMS.atomCivilKindRepeated;

        case cctz::time_zone::civil_lookup::SKIPPED:
            return ATOMS.atomCivilKindSkipped;

        default:
            return ATOMS.atomUndefined;
    }
}

class TimeZoneWrapper
{

public:

    TimeZoneWrapper() {}
    ~TimeZoneWrapper() {}

    bool load(const std::string& timezone);
    void load_utc_timezone();
    void load_local_timezone();
    void load_fixed_timezone(uint64_t offset);

    cctz::time_zone::civil_lookup lookup(const cctz::civil_second& sec);
    cctz::time_zone::absolute_lookup lookup(uint64_t seconds);

private:
    
    DISALLOW_COPY_AND_ASSIGN(TimeZoneWrapper);
    
    cctz::time_zone tz_;
};

bool TimeZoneWrapper::load(const std::string &timezone)
{
    return cctz::load_time_zone(timezone, &tz_);
}

void TimeZoneWrapper::load_utc_timezone()
{
    tz_ = cctz::utc_time_zone();
}

void TimeZoneWrapper::load_local_timezone()
{
    tz_ = cctz::local_time_zone();
}

void TimeZoneWrapper::load_fixed_timezone(uint64_t offset)
{
    tz_ = cctz::fixed_time_zone(std::chrono::duration<uint64_t>(offset));
}

cctz::time_zone::civil_lookup TimeZoneWrapper::lookup(const cctz::civil_second& sec)
{
    return tz_.lookup(sec);
}

cctz::time_zone::absolute_lookup TimeZoneWrapper::lookup(uint64_t seconds)
{
    return tz_.lookup(timestamp2tp(seconds));
}

}

struct enif_timezone
{
    TimeZoneWrapper* tz;
};

void enif_timezone_free(ErlNifEnv* env, void* obj)
{
    UNUSED(env);

    enif_timezone* timezone = static_cast<enif_timezone*>(obj);

    if(timezone->tz)
        delete timezone->tz;
}

ERL_NIF_TERM enif_timezone_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    elocatime_data* data = static_cast<elocatime_data*>(enif_priv_data(env));

    scoped_ptr(timezone, enif_timezone, static_cast<enif_timezone*>(enif_alloc_resource(data->res_timezone, sizeof(enif_timezone))), enif_release_resource);
    timezone->tz = new TimeZoneWrapper();

    if(enif_is_binary(env, argv[0]))
    {
        std::string named_timezone;

        if(!get_string(env, argv[0], &named_timezone))
            return make_badarg(env);

        if(!timezone->tz->load(named_timezone))
            return make_error(env, kUnknownTimezone);
    }
    else if(enif_is_atom(env, argv[0]))
    {
        if(enif_is_identical(argv[0], ATOMS.atomTimezoneLocal))
            timezone->tz->load_local_timezone();
        else if(enif_is_identical(argv[0], ATOMS.atomTimezoneUtc))
            timezone->tz->load_utc_timezone();
        else
            return make_error(env, kUnknownTimezone);
    }
    else if(enif_is_number(env, argv[0]))
    {
        unsigned long offset;

        if(!enif_get_uint64(env, argv[0], &offset))
            return make_error(env, kUnknownTimezone);

        timezone->tz->load_fixed_timezone(offset);
    }

    return make_ok_result(env, enif_make_resource(env, timezone.get()));
}

ERL_NIF_TERM enif_timezone_absolute_lookup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    elocatime_data* data = static_cast<elocatime_data*>(enif_priv_data(env));

    enif_timezone* timezone;
    unsigned long timestamp;

    if(!enif_get_resource(env, argv[1], data->res_timezone, (void**)&timezone))
        return make_badarg(env);

    if(!enif_get_uint64(env, argv[0], &timestamp))
        return make_badarg(env);

    cctz::time_zone::absolute_lookup lookup = timezone->tz->lookup(static_cast<uint64_t>(timestamp));

    ERL_NIF_TERM nif_date = enif_make_tuple3(env,
                                             enif_make_int(env, lookup.cs.year()),
                                             enif_make_int(env, lookup.cs.month()),
                                             enif_make_int(env, lookup.cs.day()));
    ERL_NIF_TERM nif_time = enif_make_tuple3(env,
                                             enif_make_int(env, lookup.cs.hour()),
                                             enif_make_int(env, lookup.cs.minute()),
                                             enif_make_int(env, lookup.cs.second()));

    return make_ok_result(env, enif_make_tuple5(env,
                                                ATOMS.atomAbsLookup,
                                                enif_make_tuple2(env, nif_date, nif_time),
                                                enif_make_int(env, lookup.offset),
                                                lookup.is_dst ? ATOMS.atomTrue : ATOMS.atomFalse,
                                                make_binary(env, lookup.abbr, strlen(lookup.abbr))));

}

ERL_NIF_TERM enif_timezone_civil_lookup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    elocatime_data* data = static_cast<elocatime_data*>(enif_priv_data(env));

    enif_timezone* timezone;

    if(!enif_get_resource(env, argv[1], data->res_timezone, (void**)&timezone))
        return make_badarg(env);

    const ERL_NIF_TERM *datetime;
    const ERL_NIF_TERM *date;
    const ERL_NIF_TERM *time;
    int arity;

    if(!enif_get_tuple(env, argv[0], &arity, &datetime) || arity != 2)
        return make_badarg(env);

    if(!enif_get_tuple(env, datetime[0], &arity, &date) || arity != 3)
        return make_badarg(env);

    if(!enif_get_tuple(env, datetime[1], &arity, &time) || arity != 3)
        return make_badarg(env);

    int year, month, day, hour, minute, second;

    if(!enif_get_int(env, date[0], &year) || !enif_get_int(env, date[1], &month) || !enif_get_int(env, date[2], &day))
       return make_badarg(env);

    if(!enif_get_int(env, time[0], &hour) || !enif_get_int(env, time[1], &minute) || !enif_get_int(env, time[2], &second))
        return make_badarg(env);

    cctz::civil_second civil_sec(year, month, day, hour, minute, second);
    cctz::time_zone::civil_lookup lookup = timezone->tz->lookup(civil_sec);

    return make_ok_result(env, enif_make_tuple5(env,
                                                ATOMS.atomCivilLookup,
                                                kind2atom(lookup.kind),
                                                enif_make_uint64(env, tp2timestamp(lookup.pre)),
                                                enif_make_uint64(env, tp2timestamp(lookup.trans)),
                                                enif_make_uint64(env, tp2timestamp(lookup.post))));
}



