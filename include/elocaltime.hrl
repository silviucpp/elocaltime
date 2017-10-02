
-define(TIMEZONE_UTC, tz_utc).
-define(TIMEZONE_LOCAL, tz_local).
-define(TIMEZONE_FIXED(Sec), {tz_fixed, Sec}).

-define(DS_STANDARD, prefer_standard).
-define(DS_DAYLIGHT, prefer_daylight).
-define(DS_BOTH, both).

-define(CIVIL_KIND_UNIQUE, unique).
-define(CIVIL_KIND_SKIPPED, skipped).
-define(CIVIL_KIND_REPEATED, repeated).

-type reason() :: any().
-type timestamp() :: non_neg_integer().
-type datetime() :: calendar:datetime() | timestamp().
-type timezone() :: binary() | ?TIMEZONE_UTC | ?TIMEZONE_LOCAL | ?TIMEZONE_FIXED(integer()).
-type disambiguation() :: ?DS_STANDARD | ?DS_DAYLIGHT | ?DS_BOTH.
-type civil_kind() :: ?CIVIL_KIND_REPEATED | ?CIVIL_KIND_UNIQUE | ?CIVIL_KIND_SKIPPED.

-record(absolute_lookup, {
    date :: calendar:datetime(),
    offset :: integer(),
    is_dst ::boolean(),
    tz_abbreviation :: binary()
}).

-record(civil_lookup, {
    civil_kind :: civil_kind(),
    pre ::timestamp(),
    trans :: timestamp(),
    post :: timestamp()
}).

-type absolute_lookup() :: #absolute_lookup{}.
-type civil_lookup() :: #civil_lookup{}.
