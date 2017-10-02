-module(elocaltime_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        proccess(elocaltime_timezone, infinity)
    ],

    {ok, {{one_for_one, 20, 1}, Children}}.

proccess(Name, WaitForClose) ->
    {Name, {Name, start_link, []}, permanent, WaitForClose, worker, [Name]}.