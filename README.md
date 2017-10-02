Erlang library for conversion from one local time to another based on [google cctz][1] 

### Getting started:

##### Integration

The application is compatible with both `rebar` or `rebar3`. Add `elocaltime` as a rebar dependency to your project:

```
{deps, [
  {elocaltime, ".*", {git, "https://github.com/silviucpp/elocaltime.git", "master"}},
}.
```

### API

All functions accepts the datetime in both `calendar:datetime()` or unix timestamp (number of seconds since 1 Jan 970) format.

Timezones can be one of the following:

- Any named timezone part of the [tz database][2]: For example : `<<"Europe/Bucharest">>`.
- Local timezone: Use `?TIMEZONE_LOCAL` (automatically detects the local timezone if not fallback on `UTC`).
- UTC : Use `?TIMEZONE_UTC` to specify the UTC timezone. 
- Fixed timezone: a time zone that is a fixed offset (seconds east) from UTC.  Note: If the absolute value of the offset is greater than 
  24 hours you'll get UTC (i.e., zero offset) instead. In order to specify this use `?TIMEZONE_FIXED(Seconds)`.

##### API to convert from one tz to another:

- `utc2local_datetime/2` – Converts UTC to local time and return the result as `calendar:datetime()`
- `utc2local_ts/2` – Converts UTC to local time and return the result as unix timestamp
- `local2utc_datetime/3` – Converts a local time to UTC and return the result as `calendar:datetime()`
- `local2utc_ts/3` – Converts a local time to UTC and return the result as unix timestamp

Note: for `local2utc_xxx/3` functions, the third parameter indicates what to return in case the date is ambiguous. Sometimes, 
when you are converting a datetime from one timezone to another, there are potentially two different results if the conversion happens 
to land on in a timezone that's in the middle of a Daylight Saving conversion.

By default disambiguation is disabled, and `elocaltime` will just guess as to it's best choice. But if you so desire, you can make 
sure elocaltime does both conversions, and returns both.

You can change this behaviour using the parameter:

- `?DS_STANDARD`: (default) If an ambiguous result occurs, will return the date in standard time rather than daylight time.
- `?DS_DAYLIGHT`: If an ambiguous result occurs, will return the preferred daylight time.
- `?DS_BOTH`: If an ambiguous result occurs both will be returned as a tuple of 3 elements `{ok, Standard, Daylight}`

##### Other public functions

###### civil_lookup 

A civil_lookup represents the absolute time(s) that correspond to the given civil time within this time_zone. 
Usually the given civil time represents a unique instant in time, in which case the 
conversion is unambiguous. However, within this time zone, the given civil time may be skipped (e.g., during a positive 
UTC offset shift), or repeated (e.g., during a negative UTC offset shift). To account for these possibilities, civil_lookup 
is richer than just a single timestamp. 

Returns the result as a record defined as follow:

```erlang
-record(civil_lookup, {
    civil_kind :: civil_kind(),
    pre ::timestamp(),
    trans :: timestamp(),
    post :: timestamp()
}).
````

Where :

- `civil_kind` - Indicate if it's unique instant in time, in which case the conversion is unambiguous or the given civil 
time may be skipped (during a positive UTC offset shift), or repeated (during a negative UTC offset shift)
- `pre` - timestamp that uses the pre-transition offset
- `trans` - timestamp representing the instant of civil-offset change
- `post` - timestamp uses the post-transition offset

Examples:

- A unique civil time

```erlang
{ok,{civil_lookup,unique,1293868800,1293868800,1293868800}} =
    elocaltime:civil_lookup({{2011, 1, 1}, {0, 0, 0}}, <<"America/Los_Angeles">>).
```

- A Spring DST transition, when there is a gap in civil time

```erlang
{ok,{civil_lookup,skipped,1300011300,1300010400,1300007700}} =
    elocaltime:civil_lookup({{2011, 3, 13}, {2, 15, 0}}, <<"America/Los_Angeles">>)
````

 In this example `pre` is `2011/03/13 03:15:00 -0700`, `trans` is `2011/03/13 03:00:00 -0700` and `post` is `2011/03/13 01:15:00 -0800`

- A Fall DST transition, when civil times are repeated.

```erlang
{ok,{civil_lookup,repeated,1320567300,1320570000,1320570900}} =
    elocaltime:civil_lookup({{2011, 11, 6}, {1, 15, 0}}, <<"America/Los_Angeles">>).
```    

In this example `pre` is `2011/11/06 01:15:00 -0700`, `trans` is `2011/11/06 01:00:00 -0800` and `post` is `2011/11/06 01:15:00 -0800`

###### absolute_lookup

An absolute_lookup represents the civil time within this timezone at the given absolute time. There are additionally a 
few other fields that may be useful as well. The result is returned as `absolute_lookup` record:

```erlang
-record(absolute_lookup, {
    date :: calendar:datetime(),
    offset :: integer(),
    is_dst ::boolean(),
    tz_abbreviation :: binary()
}). 
``` 

Where:

- `date` : the datetime in the new timezone
- `offset`: the timezone offset
- `is_dst`: indicates if the offset is non-standard
- `tz_abbreviation`: time-zone abbreviation (e.g., `<<"PST">>`)

### Tests

In order to run the tests just use `make ct` from project root.

[1]:https://github.com/google/cctz
[2]:https://www.iana.org/time-zones
