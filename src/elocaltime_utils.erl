-module(elocaltime_utils).

-export([
    safe_call/2,
    safe_call/3,
    ts2datetime/1,
    datetime2ts/1
]).

%% Days between Jan 1, 0001 (beginning of the Gregorian calendar) and Jan 1,
%% 1970 (Unix epoch) in seconds.  62167219200 = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).

-define(GREGORIAN_SECONDS_TO_UNIX_EPOCH, 62167219200).

safe_call(Receiver, Message) ->
    safe_call(Receiver, Message, 5000).

safe_call(Receiver, Message, Timeout) ->
    try
        gen_server:call(Receiver, Message, Timeout)
    catch
        exit:{noproc, _} ->
            {error, not_started};
        _: Exception ->
            {error, Exception}
    end.

ts2datetime(Seconds) ->
    SecsGregToEpoch = ?GREGORIAN_SECONDS_TO_UNIX_EPOCH,
    calendar:gregorian_seconds_to_datetime(SecsGregToEpoch + Seconds).

datetime2ts(DateTime) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    Seconds - ?GREGORIAN_SECONDS_TO_UNIX_EPOCH.
