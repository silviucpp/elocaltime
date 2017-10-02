-module(elocaltime_timezone).

-include("elocaltime.hrl").

-behaviour(gen_server).

-export([
    start_link/0,
    get/1,

    % gen_server

    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

get(Timezone) ->
    case elocaltime_tz_cache:get(Timezone) of
        {ok, _TzRef} = Result ->
            Result;
        undefined ->
            case get_timezone(Timezone) of
                {ok, Tz} ->
                    elocaltime_utils:safe_call(?MODULE, {get_timezone, Tz}, infinity);
                Error ->
                    Error
            end
    end.

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({get_timezone, Timezone}, _From, State) ->
    case elocaltime_tz_cache:get(Timezone) of
        {ok, _TzRef} = Result ->
            {reply, Result, State};
        undefined ->
            case elocaltime_nif:new_timezone(Timezone) of
                {ok, TzRef} = Result ->
                    ok = elocaltime_tz_cache:set(Timezone, TzRef),
                    {reply, Result, State};
                Error ->
                    {reply, Error, State}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_timezone(Tz) when is_binary(Tz) ->
    {ok, Tz};
get_timezone(?TIMEZONE_UTC) ->
    {ok, ?TIMEZONE_UTC};
get_timezone(?TIMEZONE_LOCAL) ->
    {ok, ?TIMEZONE_LOCAL};
get_timezone(?TIMEZONE_FIXED(Sec)) when is_integer(Sec) ->
    {ok, Sec};
get_timezone(_) ->
    {error, <<"invalid timezone format">>}.
