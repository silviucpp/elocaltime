-module(elocaltime_tz_cache).

-export([
    create/0,
    set/2,
    get/1
]).

-define(ETS_TIMEZONE_CACHE, elocaltime_tz_cache_tab).
-define(GET_KEY(ClientId, TopicName), {ClientId, TopicName}).

create() ->
    ?ETS_TIMEZONE_CACHE = ets:new(?ETS_TIMEZONE_CACHE, [set, named_table, public, {read_concurrency, true}]),
    ok.

set(Timezone, TimezoneRef) ->
    true = ets:insert(?ETS_TIMEZONE_CACHE, {Timezone, TimezoneRef}),
    ok.

get(Timezone) ->
    case ets:lookup(?ETS_TIMEZONE_CACHE, Timezone) of
        [{Timezone, TimezoneRef}] ->
            {ok, TimezoneRef};
        [] ->
            undefined
    end.
