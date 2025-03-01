[![Build Status](https://app.travis-ci.com/silviucpp/elocaltime.svg?branch=master)](https://travis-ci.com/github/silviucpp/elocaltime)
[![GitHub License](https://img.shields.io/github/license/silviucpp/elocaltime)](https://github.com/silviucpp/elocaltime/blob/master/LICENSE)
[![Hex.pm Version](https://img.shields.io/hexpm/v/elocaltime)](https://hex.pm/packages/elocaltime)

Erlang library for converting between local times using [Google CCTZ][1].

## Getting Started

### Integration

To add `elocaltime` to your project, include it as a dependency in `rebar3`:

```erlang
{deps, [
  {elocaltime, ".*", {git, "https://github.com/silviucpp/elocaltime.git", "master"}}
]}.
```

### API

All public functions support both `calendar:datetime()` and Unix timestamps (seconds since Jan 1, 1970) as input formats.

#### Timezone Specification

Timezones can be defined in the following ways:

- **Named timezones** from the [TZ database][2], e.g., `<<"Europe/Bucharest">>`.
- **Local timezone** using `?TIMEZONE_LOCAL` (automatically detects the local timezone if not fallback on `UTC`).
- **UTC timezone** specified with `?TIMEZONE_UTC`.
- **Fixed time offset from UTC** using `?TIMEZONE_FIXED(Seconds)`. If the absolute offset exceeds 24 hours, it defaults to UTC.

### Converting Between Timezones

- `utc2local_datetime/2` – Converts UTC to local time, returning `calendar:datetime()`.
- `utc2local_ts/2` – Converts UTC to local time, returning a Unix timestamp.
- `local2utc_datetime/3` – Converts local time to UTC, returning `calendar:datetime()`.
- `local2utc_ts/3` – Converts local time to UTC, returning a Unix timestamp.

#### Handling Ambiguous Times

For `local2utc_xxx/3` functions, the third parameter indicates what to return in case the date is ambiguous. Sometimes, 
when you are converting a datetime from one timezone to another, there are potentially two different results if the conversion happens 
to land on in a timezone that's in the middle of a Daylight Saving conversion.

By default, disambiguation is disabled, and `elocaltime` will just guess as to it's best choice. But if you so desire, you can make 
sure elocaltime does both conversions, and returns both.

You can change this behaviour using the parameter:

- `?DS_STANDARD`: (default) If an ambiguous result occurs, will return the date in standard time rather than daylight time.
- `?DS_DAYLIGHT`: If an ambiguous result occurs, will return the preferred daylight time.
- `?DS_BOTH`: If an ambiguous result occurs both will be returned as a tuple of 3 elements `{ok, Standard, Daylight}`.

### Date Formatting

This API Formats the given `DateTime` in the given timezone according to the provided format string. Uses [strftime()-like][3] formatting options,
with the following extensions:

- `%Ez` – RFC3339-compatible numeric UTC offset (`+hh:mm` or `-hh:mm`).
- `%E*z` – Full-resolution numeric UTC offset (`+hh:mm:ss` or `-hh:mm:ss`).
- `%E#S` – Seconds with # digits of fractional precision.
- `%E*S` – Seconds with full fractional precision (a literal `*`).
- `%E#f` – Fractional seconds with `#` precision.
- `%E*f` – Fractional seconds with full precision (a literal `*`).
- `%E4Y` – Four-character years (-999 ... -001, 0000, 0001 ... 9999).

#### Notes

- `%E0S` behaves like `%S`, and `%E0f` produces no characters. In contrast `%E*f` always produces at least one digit, which may be '0'.
- `%Y` produces as many characters as it takes to fully render the year. A year outside of [-999:9999] when formatted with `%E4Y` will produce more than four characters, just like `%Y`.

### Additional Public Functions

#### civil_lookup/2

A civil_lookup represents the absolute time(s) that correspond to the given civil time within this time_zone. 
Usually the given civil time represents a unique instant in time, in which case the 
conversion is unambiguous. However, within this time zone, the given civil time may be skipped (e.g., during a positive 
UTC offset shift), or repeated (e.g., during a negative UTC offset shift). To account for these possibilities, civil_lookup 
is richer than just a single timestamp. 

Returns the result as a record defined as follows:

```erlang
-record(civil_lookup, {
    civil_kind :: civil_kind(),
    pre :: timestamp(),
    trans :: timestamp(),
    post :: timestamp()
}).
```
Where :

- `civil_kind` - Indicate if it's unique instant in time, in which case the conversion is unambiguous or the given civil time may be skipped (during a positive UTC offset shift), or repeated (during a negative UTC offset shift)
- `pre` - timestamp that uses the pre-transition offset
- `trans` - timestamp representing the instant of civil-offset change
- `post` - timestamp uses the post-transition offset

##### Examples

**Unique Civil Time:**

```erlang
{ok,{civil_lookup,unique,1293868800,1293868800,1293868800}} =
    elocaltime:civil_lookup({{2011, 1, 1}, {0, 0, 0}}, <<"America/Los_Angeles">>).
```

**Spring DST Transition (Time Skipped):**

```erlang
{ok,{civil_lookup,skipped,1300011300,1300010400,1300007700}} =
    elocaltime:civil_lookup({{2011, 3, 13}, {2, 15, 0}}, <<"America/Los_Angeles">>).
```

**Fall DST Transition (Time Repeated):**

```erlang
{ok,{civil_lookup,repeated,1320567300,1320570000,1320570900}} =
    elocaltime:civil_lookup({{2011, 11, 6}, {1, 15, 0}}, <<"America/Los_Angeles">>).
```

In this example `pre` is `2011/11/06 01:15:00 -0700`, `trans` is `2011/11/06 01:00:00 -0800` and `post` is `2011/11/06 01:15:00 -0800`

#### absolute_lookup/2

An absolute_lookup represents the civil time within this timezone at the given absolute time. There are additionally a 
few other fields that may be useful as well. The result is returned as `absolute_lookup` record:

```erlang
-record(absolute_lookup, {
    date :: calendar:datetime(),
    offset :: integer(),
    is_dst :: boolean(),
    tz_abbreviation :: binary()
}).
```

Where:

- `date` – Datetime in the target timezone.
- `offset` – Timezone offset in seconds.
- `is_dst` – Indicates if the offset is non-standard.
- `tz_abbreviation` – Timezone abbreviation (e.g., `<<"PST">>`).

## Running Tests

Execute tests using:

```sh
rebar3 ct
```

[1]: https://github.com/google/cctz
[2]: https://www.iana.org/time-zones
[3]: http://www.cplusplus.com/reference/ctime/strftime/
