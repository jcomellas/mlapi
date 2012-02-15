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

-export([now_to_epoch_float/0, now_to_epoch_float/1,
         universal_time_to_epoch_float/0,
         epoch_float_to_datetime/1, datetime_to_epoch_float/1,
         iso8601_to_datetime/1, datetime_to_iso8601/1,
         iso8601_to_gregorian_seconds/1, gregorian_seconds_to_iso8601/1,
         iso8601_to_epoch_seconds/1, epoch_seconds_to_iso8601/1]).

-export_type([epoch_float/0]).


%% A floating point number representing the number of seconds elapsed since
%% Jan 1, 1970, 00:00:00 (Unix epoch).
-type epoch_float()                             :: float().

%% Days between Jan 1, 0001 (beginning of the Gregorian calendar) and Jan 1, 1970 (Unix epoch) in seconds.
%% 62167219200 = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(SECONDS_TO_UNIX_EPOCH, 62167219200).


%% @doc Returns the current date and time as a floating-point timestamp with
%%      the number of seconds since the Unix Epoch (Jan 1, 1970, 00:00:00) and
%%      a precision of microseconds, as returned by erlang:now/0.
-spec now_to_epoch_float() -> epoch_float().
now_to_epoch_float() ->
    now_to_epoch_float(erlang:now()).


%% @doc Converts the date and time inf the format returned by erlang:now/0 to a
%%      floating-point timestamp with the number of seconds since the Unix Epoch
%%      (Jan 1, 1970, 00:00:00) and a precision of microseconds.
-spec now_to_epoch_float({Megasecs :: non_neg_integer(), Secs :: non_neg_integer(),
                          Microsecs :: non_neg_integer()}) -> epoch_float().
now_to_epoch_float({Megasecs, Secs, Microsecs}) ->
    (Megasecs * 1000000 + Secs) + Microsecs / 1000000.


%% @doc Returns the date and time as a floating-point timestamp with
%%      the number of seconds since the Unix Epoch (Jan 1, 1970, 00:00:00).
-spec universal_time_to_epoch_float() -> epoch_float().
universal_time_to_epoch_float() ->
    datetime_to_epoch_float(calendar:universal_time()).


%% @doc Convert a timestamp as a floating-point with the number of seconds since
%%      the Unix Epoch (Jan 1, 1970, 00:00:00) and a precision of microseconds
%%      into a datetime in the format returned by the calendar:universal_time/0 function.
-spec epoch_float_to_datetime(epoch_float()) -> calendar:datetime1970().
epoch_float_to_datetime(Epoch_Float) ->
    calendar:gregorian_seconds_to_datetime(round(Epoch_Float) + ?SECONDS_TO_UNIX_EPOCH).


%% @doc Convert a datetime in the format returned by the calendar:universal_time/0 function
%%      into a timestamp as a floating-point with the number of seconds since
%%      the Unix Epoch (Jan 1, 1970, 00:00:00) and a precision of microseconds.
-spec datetime_to_epoch_float(calendar:datetime1970()) -> epoch_float().
datetime_to_epoch_float({{_Year, _Month, _Day}, {_Hour, _Min, _Sec}} = Datetime) ->
    float(calendar:datetime_to_gregorian_seconds(Datetime) - ?SECONDS_TO_UNIX_EPOCH).


%% @doc Convert a datetime in the ISO 8601 format to a date and time in the
%%      format returned by calendar:universal_time/0.
-spec iso8601_to_datetime(binary()) -> calendar:datetime().
iso8601_to_datetime(<<Year:4/binary, $-, Month:2/binary, $-, Day:2/binary, $T,
                       Hour:2/binary, $:, Min:2/binary, $:, Sec:2/binary, $., _Millisecs:3/binary, $Z>>) ->
    {{bstr:to_integer(Year), bstr:to_integer(Month), bstr:to_integer(Day)},
     {bstr:to_integer(Hour), bstr:to_integer(Min), bstr:to_integer(Sec)}};
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
    calendar:gregorian_seconds_to_datetime(UtcSecs + round(Millisecs / 1000)).


%% @doc Convert a date and time in the format returned by calendar:universal_time/0 to
%%      a binary string in the ISO 8601 format (e.g. "2012-02-15T14:39:15Z").
-spec datetime_to_iso8601(calendar:datetime()) -> binary().
datetime_to_iso8601({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    YYYY = bstr:lpad(bstr:from_integer(Year), 4, $0),
    MM = bstr:lpad(bstr:from_integer(Month), 2, $0),
    DD = bstr:lpad(bstr:from_integer(Day), 2, $0),
    Hh = bstr:lpad(bstr:from_integer(Hour), 2, $0),
    Mm = bstr:lpad(bstr:from_integer(Min), 2, $0),
    Ss = bstr:lpad(bstr:from_integer(Sec), 2, $0),
    <<YYYY/binary, $-, MM/binary, $-, DD/binary, $T, Hh/binary, $:, Mm/binary, $:, Ss/binary, $Z>>.


%% @doc Convert a date and time as binary string in the ISO 8601 format to the
%%      number of seconds since the Jan 1, 0001, 00:00:00.
%%      e.g. "2012-02-15T14:39:15.345Z"; "2012-02-15T14:39:15Z"; "2012-02-15T14:39:15.345-0300".
-spec iso8601_to_gregorian_seconds(binary()) -> non_neg_integer().
iso8601_to_gregorian_seconds(<<Year:4/binary, $-, Month:2/binary, $-, Day:2/binary, $T,
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
    UtcSecs + round(Millisecs / 1000);
iso8601_to_gregorian_seconds(Iso8601) ->
    calendar:datetime_to_gregorian_seconds(iso8601_to_datetime(Iso8601)).


%% @doc Convert a date and time as the number of seconds since Jan 1, 0001, 00:00:00
%%      to a binary string in the ISO 8601 format (e.g. "2012-02-15T14:39:15Z").
-spec gregorian_seconds_to_iso8601(non_neg_integer()) -> binary().
gregorian_seconds_to_iso8601(Seconds) ->
    datetime_to_iso8601(calendar:gregorian_seconds_to_datetime(Seconds)).


%% @doc Convert a date and time as binary string in the ISO 8601 format to the
%%      number of seconds since the Unix epoch (Jan 1, 1970, 00:00:00).
-spec iso8601_to_epoch_seconds(binary()) -> non_neg_integer().
iso8601_to_epoch_seconds(Iso8601) ->
    iso8601_to_gregorian_seconds(Iso8601) - ?SECONDS_TO_UNIX_EPOCH.


%% @doc Convert a date and time as the number of seconds since the Unix epoch
%%      (Jan 1, 1970, 00:00:00) to a binary string in the ISO 8601 format
%%      (e.g. "2012-02-15T14:39:15Z").
-spec epoch_seconds_to_iso8601(non_neg_integer()) -> binary().
epoch_seconds_to_iso8601(Seconds) ->
    datetime_to_iso8601(calendar:gregorian_seconds_to_datetime(Seconds + ?SECONDS_TO_UNIX_EPOCH)).
