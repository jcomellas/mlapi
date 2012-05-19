%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2012 Juan Jose Comellas
%%% @doc Time helper functions.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(mlapi_time).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([datetime_to_iso8601/1, iso8601_to_datetime/1,
         gregorian_seconds_to_iso8601/1, iso8601_to_gregorian_seconds/1,
         epoch_to_iso8601/1, iso8601_to_epoch/1,
         datetime_to_epoch/1, epoch_to_datetime/1,
         timestamp_to_epoch/0, timestamp_to_epoch/1,
         universal_time_to_epoch/0]).

-export_type([datetime/0, epoch/0]).


%% Tuple containing a date and time.
-type datetime()                                :: {calendar:date(), {calendar:hour(), calendar:minute(), calendar:second() | float()}}.
%% A floating point number representing the number of seconds elapsed since
%% Jan 1, 1970, 00:00:00 (Unix epoch).
-type epoch()                                   :: non_neg_integer() | float().

%% Days between Jan 1, 0001 (beginning of the Gregorian calendar) and Jan 1, 1970 (Unix epoch) in seconds.
%% 62167219200 = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(SECONDS_TO_UNIX_EPOCH, 62167219200).


%% @doc Convert a date and time in the format returned by calendar:universal_time/0 to
%%      a binary string in the ISO 8601 format (e.g. "2012-02-15T14:39:15Z"; "2012-02-15T14:39:15.671Z").
-spec datetime_to_iso8601(calendar:datetime()) -> binary().
datetime_to_iso8601({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    YYYY = bstr:lpad(bstr:from_integer(Year), 4, $0),
    MM = bstr:lpad(bstr:from_integer(Month), 2, $0),
    DD = bstr:lpad(bstr:from_integer(Day), 2, $0),
    Hh = bstr:lpad(bstr:from_integer(Hour), 2, $0),
    Mm = bstr:lpad(bstr:from_integer(Min), 2, $0),
    if
        is_integer(Sec) ->
            Ss = bstr:lpad(bstr:from_integer(Sec), 2, $0),
            <<YYYY/binary, $-, MM/binary, $-, DD/binary, $T, Hh/binary, $:, Mm/binary, $:, Ss/binary, $Z>>;
        is_float(Sec) ->
            IntSec = trunc(Sec),
            Ss = bstr:lpad(bstr:from_integer(IntSec), 2, $0),
            Ms = bstr:lpad(bstr:from_integer(round((Sec - IntSec) * 1000)), 3, $0),
            <<YYYY/binary, $-, MM/binary, $-, DD/binary, $T, Hh/binary, $:, Mm/binary, $:, Ss/binary, $., Ms/binary, $Z>>
    end.


%% @doc Convert a datetime in the ISO 8601 format to a date and time in the
%%      format returned by calendar:universal_time/0.
-spec iso8601_to_datetime(binary()) -> datetime().
iso8601_to_datetime(<<Year:4/binary, $-, Month:2/binary, $-, Day:2/binary, $T,
                       Hour:2/binary, $:, Min:2/binary, $:, Sec:2/binary, $., Millisecs:3/binary, $Z>>) ->
    IntSec = bstr:to_integer(Sec),
    FullSec = case Millisecs of
                  <<"000">> -> IntSec;
                  _         -> IntSec + bstr:to_integer(Millisecs) / 1000.0
              end,
    {{bstr:to_integer(Year), bstr:to_integer(Month), bstr:to_integer(Day)},
     {bstr:to_integer(Hour), bstr:to_integer(Min), FullSec}};
iso8601_to_datetime(<<Year:4/binary, $-, Month:2/binary, $-, Day:2/binary, $T,
                       Hour:2/binary, $:, Min:2/binary, $:, Sec:2/binary, $Z>>) ->
    {{bstr:to_integer(Year), bstr:to_integer(Month), bstr:to_integer(Day)},
     {bstr:to_integer(Hour), bstr:to_integer(Min), bstr:to_integer(Sec)}};
iso8601_to_datetime(<<Year:4/binary, $-, Month:2/binary, $-, Day:2/binary, $T,
                       Hour:2/binary, $:, Min:2/binary, $:, Sec:2/binary, $., Millisecs:3/binary, Sign,
                       TimezoneHour:2/binary, $:, TimezoneMin:2/binary>>) ->
    LocalSecs = calendar:datetime_to_gregorian_seconds({{bstr:to_integer(Year), bstr:to_integer(Month), bstr:to_integer(Day)},
                                                        {bstr:to_integer(Hour), bstr:to_integer(Min), bstr:to_integer(Sec)}}),
    %% Convert the the seconds in the local timezone to UTC.
    UtcSecs = case ((bstr:to_integer(TimezoneHour) * 60 + bstr:to_integer(TimezoneMin)) * 60) of
                  Offset when Sign =:= $- ->
                      LocalSecs - Offset;
                  Offset ->
                      LocalSecs + Offset
              end,
    {Date, {Hour, Min, Sec}} = Datetime = calendar:gregorian_seconds_to_datetime(UtcSecs),
    case bstr:to_integer(Millisecs) of
        0 ->
            Datetime;
        _ ->
            {Date, {Hour, Min, Sec + Millisecs / 1000.0}}
    end.


%% @doc Convert a date and time as the number of seconds since Jan 1, 0001, 00:00:00
%%      to a binary string in the ISO 8601 format (e.g. "2012-02-15T14:39:15Z").
-spec gregorian_seconds_to_iso8601(non_neg_integer()) -> binary().
gregorian_seconds_to_iso8601(Seconds) ->
    datetime_to_iso8601(calendar:gregorian_seconds_to_datetime(Seconds)).


%% @doc Convert a date and time as binary string in the ISO 8601 format to the
%%      number of seconds since the Jan 1, 0001, 00:00:00.
%%      e.g. "2012-02-15T14:39:15.345Z"; "2012-02-15T14:39:15Z"; "2012-02-15T14:39:15.345-0300".
-spec iso8601_to_gregorian_seconds(binary()) -> non_neg_integer().
iso8601_to_gregorian_seconds(Iso8601) ->
    Datetime = case iso8601_to_datetime(Iso8601) of
                   {Date, {Hour, Min, Sec}} when is_float(Sec) ->
                       {Date, {Hour, Min, round(Sec)}};
                   Value ->
                       Value
               end,
    calendar:datetime_to_gregorian_seconds(Datetime).


%% @doc Convert a date and time as the number of seconds since the Unix epoch
%%      (Jan 1, 1970, 00:00:00) with millisecond precision to a binary string
%%      in the ISO 8601 format (e.g. "2012-02-15T14:39:15.539Z").
-spec epoch_to_iso8601(epoch()) -> binary().
epoch_to_iso8601(Epoch) ->
    TruncatedEpoch = trunc(Epoch),
    Datetime = case calendar:gregorian_seconds_to_datetime(TruncatedEpoch + ?SECONDS_TO_UNIX_EPOCH) of
                   {Date, {Hour, Min, Sec}} when is_float(Epoch) ->
                       {Date, {Hour, Min, Sec + (Epoch - TruncatedEpoch)}};
                   Value ->
                       Value
               end,
    datetime_to_iso8601(Datetime).


%% @doc Convert a date and time as binary string in the ISO 8601 format to the
%%      number of seconds since the Unix epoch (Jan 1, 1970, 00:00:00) with
%%      millisecond precision.
-spec iso8601_to_epoch(binary()) -> epoch().
iso8601_to_epoch(Iso8601) ->
    GregorianSecs = case iso8601_to_datetime(Iso8601) of
                        {Date, {Hour, Min, Sec}} when is_float(Sec) ->
                            TruncatedSec = trunc(Sec),
                            calendar:datetime_to_gregorian_seconds({Date, {Hour, Min, TruncatedSec}}) + (Sec - TruncatedSec);
                        Datetime ->
                            calendar:datetime_to_gregorian_seconds(Datetime)
                    end,
    GregorianSecs - ?SECONDS_TO_UNIX_EPOCH.


%% @doc Returns the current date and time as a floating-point timestamp with
%%      the number of seconds since the Unix Epoch (Jan 1, 1970, 00:00:00) and
%%      a precision of microseconds, as returned by erlang:now/0.
-spec timestamp_to_epoch() -> epoch().
timestamp_to_epoch() ->
    timestamp_to_epoch(os:timestamp()).


%% @doc Converts the date and time inf the format returned by os:timestamp/0 and
%%      erlang:now/0 ({Megasecs, Secs. Microsecs}) to a floating-point timestamp
%%      with the number of seconds since the Unix Epoch (Jan 1, 1970, 00:00:00)
%%      and a precision of microseconds.
-spec timestamp_to_epoch({Megasecs :: non_neg_integer(), Secs :: non_neg_integer(),
                          Microsecs :: non_neg_integer()}) -> epoch().
timestamp_to_epoch({Megasecs, Secs, Microsecs}) ->
    (Megasecs * 1000000 + Secs) + Microsecs / 1000000.0.


%% @doc Returns the date and time as a floating-point timestamp with
%%      the number of seconds since the Unix Epoch (Jan 1, 1970, 00:00:00).
-spec universal_time_to_epoch() -> epoch().
universal_time_to_epoch() ->
    datetime_to_epoch(calendar:universal_time()).


%% @doc Convert a datetime in the format returned by the calendar:universal_time/0 function
%%      into a timestamp as a floating-point with the number of seconds since
%%      the Unix Epoch (Jan 1, 1970, 00:00:00) and a precision of microseconds.
-spec datetime_to_epoch(datetime()) -> epoch().
datetime_to_epoch({{_Year, _Month, _Day}, {_Hour, _Min, Sec}} = Datetime) when is_integer(Sec) ->
    calendar:datetime_to_gregorian_seconds(Datetime) - ?SECONDS_TO_UNIX_EPOCH;
datetime_to_epoch({{_Year, _Month, _Day} = Date, {Hour, Min, Sec}}) when is_float(Sec) ->
    TruncatedSec = trunc(Sec),
    float(calendar:datetime_to_gregorian_seconds({Date, {Hour, Min, TruncatedSec}}) - ?SECONDS_TO_UNIX_EPOCH) + (Sec - TruncatedSec).


%% @doc Convert a timestamp as a floating-point with the number of seconds since
%%      the Unix Epoch (Jan 1, 1970, 00:00:00) and a precision of microseconds
%%      into a datetime in the format returned by the calendar:universal_time/0 function.
-spec epoch_to_datetime(epoch()) -> datetime().
epoch_to_datetime(Epoch) when is_integer(Epoch) ->
    calendar:gregorian_seconds_to_datetime(Epoch + ?SECONDS_TO_UNIX_EPOCH);
epoch_to_datetime(Epoch) ->
    TruncatedEpoch = trunc(Epoch),
    Subsec = (Epoch - TruncatedEpoch),
    {Date, {Hour, Min, Sec}} = Datetime = calendar:gregorian_seconds_to_datetime(TruncatedEpoch + ?SECONDS_TO_UNIX_EPOCH),
    if
        %% Time calculations are performed with microsecond precision.
        Subsec < 1.0e-6 ->
            Datetime;
        true ->
            %% Avoid floating point "errors"" by rounding decimals to microsecond precision.
            FloatSec = round(Subsec * 1000000) / 1000000.0,
            {Date, {Hour, Min, Sec + FloatSec}}
    end.
