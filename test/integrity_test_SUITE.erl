-module(integrity_test_SUITE).

-include("elocaltime.hrl").

-compile(export_all).

all() -> [
    {group, elocaltime_tests}
].

groups() -> [
    {elocaltime_tests, [sequence], [
        test_timezone,
        civil_lookup,
        absolute_lookup,
        local2utc_datetime,
        local2utc_ts,
        utc2local_xx,
        is_timezone_valid
    ]}
].

init_per_suite(Config) ->
    elocaltime:start(),
    Config.

end_per_suite(_Config) ->
    elocaltime:stop().

test_timezone(_Config) ->
    {ok,{{2017,3,26},{2,0,0}}} = elocaltime:utc2local_datetime({{2017,3,26},{0,0,0}}, <<"Europe/Bucharest">>),
    {ok,{{2017,3,26},{0,0,0}}} = elocaltime:utc2local_datetime({{2017,3,26},{0,0,0}}, ?TIMEZONE_UTC),
    {ok, _} = elocaltime:utc2local_datetime({{2017,3,26},{0,0,0}}, ?TIMEZONE_LOCAL),
    {ok,{{2017,3,26},{0,2,0}}} = elocaltime:utc2local_datetime({{2017,3,26},{0,0,0}}, ?TIMEZONE_FIXED(120)),
    {ok,{{2017,3,25},{23,58,0}}} = elocaltime:utc2local_datetime({{2017,3,26},{0,0,0}}, ?TIMEZONE_FIXED(-120)),
    {error, <<"unknown timezone">>} = elocaltime:utc2local_datetime({{2017,3,26},{0,0,0}}, <<"Invalid/timezone">>),
    true.

civil_lookup(_Config) ->
    Tz = <<"America/Los_Angeles">>,
    {ok, #civil_lookup{civil_kind = ?CIVIL_KIND_UNIQUE, pre = 1293868800, trans = 1293868800, post = 1293868800}} =
        elocaltime:civil_lookup({{2011, 1, 1}, {0, 0, 0}}, Tz),
    {ok,#civil_lookup{civil_kind = ?CIVIL_KIND_SKIPPED, pre = 1300011300, trans = 1300010400, post = 1300007700}} =
        elocaltime:civil_lookup({{2011, 3, 13}, {2, 15, 0}}, Tz),
    {ok,#civil_lookup{civil_kind = ?CIVIL_KIND_REPEATED, pre = 1320567300, trans = 1320570000, post = 1320570900}} =
        elocaltime:civil_lookup({{2011, 11, 6}, {1, 15, 0}}, Tz),
    true.

absolute_lookup(_Config) ->
    Timezone = <<"Europe/Bucharest">>,
    {ok,#absolute_lookup{date = {{2017,3,26},{2,0,0}}, offset = 7200, is_dst =  false, tz_abbreviation = <<"EET">>}} =
        elocaltime:absolute_lookup({{2017,3,26},{0,0,0}}, Timezone),
    {ok,#absolute_lookup{date = {{2017,3,26},{4,0,0}}, offset = 10800, is_dst = true, tz_abbreviation = <<"EEST">>}} =
        elocaltime:absolute_lookup({{2017,3,26},{1,0,0}}, Timezone),
    true.

local2utc_datetime(_Config) ->
    Date = {{2017,3,26},{3,0,0}},
    Timezone = <<"Europe/Bucharest">>,
    {ok,{{2017,3,26},{1,0,0}},{{2017,3,26},{0,0,0}}} = elocaltime:local2utc_datetime(Date, Timezone, ?DS_BOTH),
    {ok,{{2017,3,26},{1,0,0}}} = elocaltime:local2utc_datetime(Date, Timezone, ?DS_STANDARD),
    {ok,{{2017,3,26},{0,0,0}}} = elocaltime:local2utc_datetime(Date, Timezone, ?DS_DAYLIGHT),

    {ok,{{2017,5,26},{0,0,0}}} = elocaltime:local2utc_datetime({{2017,5,26},{3,0,0}}, Timezone, ?DS_BOTH),
    {ok,{{2017,5,26},{0,0,0}}} = elocaltime:local2utc_datetime({{2017,5,26},{3,0,0}}, Timezone, ?DS_STANDARD),
    {ok,{{2017,5,26},{0,0,0}}} = elocaltime:local2utc_datetime({{2017,5,26},{3,0,0}}, Timezone, ?DS_DAYLIGHT),
    true.

local2utc_ts(_Config) ->
    Date = 1490497200,
    Timezone = <<"Europe/Bucharest">>,
    {ok, 1490490000, 1490486400} = elocaltime:local2utc_ts(Date, Timezone, ?DS_BOTH),
    {ok, 1490490000} = elocaltime:local2utc_ts(Date, Timezone, ?DS_STANDARD),
    {ok, 1490486400} = elocaltime:local2utc_ts(Date, Timezone, ?DS_DAYLIGHT),

    {ok, 1495756800} = elocaltime:local2utc_ts(1495767600, Timezone, ?DS_BOTH),
    {ok, 1495756800} = elocaltime:local2utc_ts(1495767600, Timezone, ?DS_STANDARD),
    {ok, 1495756800} = elocaltime:local2utc_ts(1495767600, Timezone, ?DS_DAYLIGHT),
    true.

utc2local_xx(_Config) ->
    Timezone = <<"Europe/Bucharest">>,
    {ok,{{2017,3,26},{2,0,0}}} = elocaltime:utc2local_datetime({{2017,3,26},{0,0,0}}, Timezone),
    {ok, 1490493600} = elocaltime:utc2local_ts(1490486400, Timezone),
    true.

is_timezone_valid(_Config) ->
    true = elocaltime:is_timezone_valid(<<"Europe/Bucharest">>),
    true = elocaltime:is_timezone_valid(?TIMEZONE_LOCAL),
    true = elocaltime:is_timezone_valid(?TIMEZONE_UTC),
    true = elocaltime:is_timezone_valid(?TIMEZONE_FIXED(120)),
    true = elocaltime:is_timezone_valid(?TIMEZONE_FIXED(-120)),
    false = elocaltime:is_timezone_valid(<<"EEST">>),
    true.