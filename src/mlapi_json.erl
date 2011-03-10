%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2009 Juan Jose Comellas
%%% @doc Encode/decode JSON messages from MLAPI.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(mlapi_json).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([encode/1, decode/1]).

-type key() :: binary().
-type value() :: any().
-type proplist() :: [{key(), value() | proplist()}].
-type json() :: binary().


-spec encode(proplist()) -> json().
encode(PropList) ->
    json:encode(PropList).


-spec decode(json()) -> proplist().
decode(Json) when is_binary(Json) ->
    json:decode(Json).
