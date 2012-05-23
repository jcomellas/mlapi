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
         datetime_to_epoch/1, universal_time_to_epoch/0, epoch_to_datetime/1,
         epoch_to_iso8601/1, iso8601_to_epoch/1,
         timestamp_to_epoch/0, timestamp_to_epoch/1, timestamp_to_datetime/1]).

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
            TruncatedSec = trunc(Sec),
            Ss = bstr:lpad(bstr:from_integer(TruncatedSec), 2, $0),
            Ms = bstr:lpad(bstr:from_integer(round((Sec - TruncatedSec) * 1000.0)), 3, $0),
            <<YYYY/binary, $-, MM/binary, $-, DD/binary, $T, Hh/binary, $:, Mm/binary, $:, Ss/binary, $., Ms/binary, $Z>>
    end.


%% @doc Convert a datetime in the ISO 8601 format to a date and time in the
%%      format returned by calendar:universal_time/0.
-spec iso8601_to_datetime(binary()) -> datetime().
iso8601_to_datetime(<<YYYY:4/binary, $-, MM:2/binary, $-, DD:2/binary, $T,
                      Hh:2/binary, $:, Mm:2/binary, $:, Ss:2/binary, Tail/binary>>) ->
    Date1 = {bstr:to_integer(YYYY), bstr:to_integer(MM), bstr:to_integer(DD)},
    Hour1 = bstr:to_integer(Hh),
    Min1 = bstr:to_integer(Mm),
    Sec1 = bstr:to_integer(Ss),

    case Tail of
        <<"Z">> ->
            {Date1, {Hour1, Min1, Sec1}};
        <<".000Z">> ->
            {Date1, {Hour1, Min1, Sec1}};
        <<$., Millisec:3/binary, $Z>> ->
            {Date1, {Hour1, Min1, float(Sec1) + float(bstr:to_integer(Millisec)) / 1000.0}};
        <<$., Millisec:3/binary, Sign, TimezoneHour:2/binary, $:, TimezoneMin:2/binary>> ->
            LocalSec = calendar:datetime_to_gregorian_seconds({Date1, {Hour1, Min1, Sec1}}),
            %% Convert the the seconds in the local timezone to UTC.
            UtcSec = case ((bstr:to_integer(TimezoneHour) * 3600 + bstr:to_integer(TimezoneMin)) * 60) of
                         Offset when Sign =:= $- -> LocalSec - Offset;
                         Offset                  -> LocalSec + Offset
                     end,
            {Date, {Hour, Min, Sec}} = Datetime = calendar:gregorian_seconds_to_datetime(UtcSec),
            case Millisec of
                <<"000">> ->
                    Datetime;
                _ ->
                    {Date, {Hour, Min, float(Sec) + float(bstr:to_integer(Millisec)) / 1000.0}}
            end
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


%% @doc Convert a datetime in the format returned by the calendar:universal_time/0 function
%%      into a timestamp as a floating-point with the number of seconds since
%%      the Unix Epoch (Jan 1, 1970, 00:00:00) and a precision of microseconds.
-spec datetime_to_epoch(datetime()) -> epoch().
datetime_to_epoch({{_Year, _Month, _Day}, {_Hour, _Min, Sec}} = Datetime) when is_integer(Sec) ->
    calendar:datetime_to_gregorian_seconds(Datetime) - ?SECONDS_TO_UNIX_EPOCH;
datetime_to_epoch({{_Year, _Month, _Day} = Date, {Hour, Min, Sec}}) when is_float(Sec) ->
    TruncatedSec = trunc(Sec),
    Subsec = round((Sec - TruncatedSec) * 1000000.0) / 1000000.0,
    float(calendar:datetime_to_gregorian_seconds({Date, {Hour, Min, TruncatedSec}}) - ?SECONDS_TO_UNIX_EPOCH) + Subsec.


%% @doc Returns the date and time as a floating-point timestamp with
%%      the number of seconds since the Unix Epoch (Jan 1, 1970, 00:00:00).
-spec universal_time_to_epoch() -> epoch().
universal_time_to_epoch() ->
    datetime_to_epoch(calendar:universal_time()).


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
            FloatSec = round(Subsec * 1000000.0) / 1000000.0,
            {Date, {Hour, Min, Sec + FloatSec}}
    end.


%% @doc Convert a date and time as the number of seconds since the Unix epoch
%%      (Jan 1, 1970, 00:00:00) with millisecond precision to a binary string
%%      in the ISO 8601 format (e.g. "2012-02-15T14:39:15.539Z").
-spec epoch_to_iso8601(epoch()) -> binary().
epoch_to_iso8601(Epoch) ->
    datetime_to_iso8601(epoch_to_datetime(Epoch)).


%% @doc Convert a date and time as binary string in the ISO 8601 format to the
%%      number of seconds since the Unix epoch (Jan 1, 1970, 00:00:00) with
%%      millisecond precision.
-spec iso8601_to_epoch(binary()) -> epoch().
iso8601_to_epoch(Iso8601) ->
    datetime_to_epoch(iso8601_to_datetime(Iso8601)).


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
-spec timestamp_to_epoch(erlang:timestamp()) -> epoch().
timestamp_to_epoch({Megasecs, Secs, Microsecs}) ->
    (Megasecs * 1000000 + Secs) + Microsecs / 1000000.0.


%% @doc Converts the date and time in the format returned by os:timestamp/0 and
%%      erlang:now/0 ({Megasecs, Secs. Microsecs}) to a datetime in the format
%%      returned by the calendar:universal_time/0 function.
-spec timestamp_to_datetime(erlang:timestamp()) -> datetime().
timestamp_to_datetime(Timestamp) ->
    epoch_to_datetime(timestamp_to_epoch(Timestamp)).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

datetime_to_iso8601_test() ->
    ?assertMatch(<<"2012-05-19T22:34:55Z">>, datetime_to_iso8601({{2012,5,19},{22,34,55}})),
    ?assertMatch(<<"2012-11-30T09:01:00.486Z">>, datetime_to_iso8601({{2012,11,30},{9,1,0.486}})).

iso8601_to_datetime_test() ->
    ?assertMatch({{2012,05,19},{22,34,55}}, iso8601_to_datetime(<<"2012-05-19T22:34:55Z">>)),
    ?assertMatch({{2012,11,30},{9,1,0.486}}, iso8601_to_datetime(<<"2012-11-30T09:01:00.486Z">>)).

gregorian_seconds_to_iso8601_test() ->
    GregorianSec = calendar:datetime_to_gregorian_seconds({{2012,10,5},{1,10,11}}),
    ?assertMatch(<<"2012-10-05T01:10:11Z">>, gregorian_seconds_to_iso8601(GregorianSec)).

iso8601_to_gregorian_seconds_test() ->
    GregorianSec = calendar:datetime_to_gregorian_seconds({{1950,2,22},{15,30,14}}),
    ?assertEqual(GregorianSec + 1, iso8601_to_gregorian_seconds(<<"1950-02-22T15:30:14.653Z">>)).

datetime_to_epoch_test() ->
    Datetime = {{2000,1,1},{10,20,30}},
    Epoch = calendar:datetime_to_gregorian_seconds(Datetime) - ?SECONDS_TO_UNIX_EPOCH,
    ?assertEqual(Epoch, datetime_to_epoch(Datetime)).

epoch_to_datetime_test() ->
    Datetime = {{2000,1,1},{10,20,30}},
    Epoch = calendar:datetime_to_gregorian_seconds(Datetime) - ?SECONDS_TO_UNIX_EPOCH,
    ?assertEqual(Datetime, epoch_to_datetime(Epoch)).

epoch_to_iso8601_test() ->
    Epoch = calendar:datetime_to_gregorian_seconds({{2011, 12, 31},{5,25,53}}) - ?SECONDS_TO_UNIX_EPOCH,
    ?assertMatch(<<"2011-12-31T05:25:53Z">>, epoch_to_iso8601(Epoch)),
    ?assertMatch(<<"2011-12-31T05:25:53.672Z">>, epoch_to_iso8601(Epoch + 0.672)).

iso8601_to_epoch_test() ->
    Epoch = calendar:datetime_to_gregorian_seconds({{1989, 7, 20},{20,30,21}}) - ?SECONDS_TO_UNIX_EPOCH,
    ?assertEqual(Epoch, iso8601_to_epoch(<<"1989-07-20T20:30:21Z">>)),
    ?assertEqual(Epoch + 0.217, iso8601_to_epoch(<<"1989-07-20T20:30:21.217Z">>)).

-endif.
