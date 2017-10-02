-module(elocaltime_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    ok = elocaltime_tz_cache:create(),
    elocaltime_sup:start_link().

stop(_State) ->
    ok.
