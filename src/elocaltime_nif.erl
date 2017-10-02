-module(elocaltime_nif).

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(load_nif/0).

-export([
    new_timezone/1,
    absolute_lookup/2,
    civil_lookup/2
]).

%% nif functions

load_nif() ->
    SoName = get_priv_path(?MODULE),
    io:format(<<"Loading library: ~p ~n">>, [SoName]),
    ok = erlang:load_nif(SoName, 0).

get_priv_path(File) ->
    case code:priv_dir(erl_uap) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(?MODULE)),
            filename:join([filename:dirname(Ebin), "priv", File]);
        Dir ->
            filename:join(Dir, File)
    end.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

new_timezone(_Timezone) ->
    ?NOT_LOADED.

absolute_lookup(_Date, _TimezoneRef) ->
    ?NOT_LOADED.

civil_lookup(_Date, _TimezoneRef) ->
    ?NOT_LOADED.
