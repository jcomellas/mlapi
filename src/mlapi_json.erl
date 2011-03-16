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
-export([get/2, compile_path/1]).

-type key() :: binary().
-type value() :: binary() | boolean() | integer() | float() | 'null'.
-type ejson() :: [{key(), value() | ejson()}].
-type json() :: binary().

-define(IS_SPACE(Char), (Char) =:= $\s orelse (Char) =:= $\t orelse (Char) =:= $\r orelse (Char) =:= $\n).

-spec encode(ejson()) -> json().
encode(EJson) ->
    json:encode(EJson).


-spec decode(json()) -> ejson().
decode(Json) ->
    json:decode(Json).


get([{index, Index} | Tail], Json) when is_integer(Index), is_list(Json) ->
    get(Tail, get_index(Index, Json));
get([{Key, Value} | Tail], Json) when is_list(Json) ->
    get(Tail, get_value(Key, Value, Json));
get([Key | Tail], Json) when is_binary(Key), is_list(Json) ->
    get(Tail, get_key(Key, Json));
get([Key | Tail], Json) ->
    get(Tail, get_key(Key, Json));
get([], Json) ->
    Json;
get(Path, Json) when is_binary(Path) ->
    get(compile_path(Path), Json).


get_value(Key, Value, [{Head} = Element | Tail]) ->
    case lists:keyfind(Key, 1, Head) of
        {Key, Value} ->
            Element;
        _  ->
            get_value(Key, Value, Tail)
    end;
get_value(_Key, _Value, []) ->
    null.

get_index(Index, [_ | _] = Element) ->
    lists:nth(Index + 1, Element);
get_index(_Index, []) ->
    null.

get_key(Key, {Element}) ->
    case lists:keyfind(Key, 1, Element) of
        {Key, Value} ->
            Value;
        false ->
            null
    end;
get_key([], Json) ->
    Json.


%% Path is an expression using a syntax similar to that of JSONPath, but
%% restricted to the following operations:
%% .
%% [n] with integer index
%% [value] with field value
%% e.g. <<"id['MLAPI'].product.component[1].name">>
-spec compile_path(Path :: binary()) -> [key() | {index, non_neg_integer()} | {key(), value()}].
compile_path(Path) ->
    compile_path(Path, [], <<>>).

compile_path(<<$., Tail/binary>>, Acc, FieldAcc) ->
    compile_path(Tail,
                 case FieldAcc of
                     <<>> ->
                         Acc;
                     _ ->
                         [FieldAcc | Acc]
                 end, <<>>);
compile_path(<<$[, Tail/binary>>, Acc, FieldAcc) ->
    compile_array(Tail, [FieldAcc | Acc]);
compile_path(<<Char, Tail/binary>>, Acc, FieldAcc) ->
    compile_path(Tail, Acc, <<FieldAcc/binary, Char>>);
compile_path(<<>>, Acc, FieldAcc) when size(FieldAcc) > 0 ->
    lists:reverse([FieldAcc | Acc]);
compile_path(<<>>, Acc, _FieldAcc) ->
    lists:reverse(Acc).

compile_array(<<$@, Tail/binary>>, Acc) ->
    compile_attr_key(Tail, Acc, <<>>);
%% Skip whitespace
compile_array(<<Char, Tail/binary>>, Acc) when ?IS_SPACE(Char) ->
    compile_array(Tail, Acc);
compile_array(Tail, Acc) ->
    compile_index(Tail, Acc, 0).

compile_attr_key(<<$=, Tail/binary>>, Acc, KeyAcc) ->
    compile_attr_value(Tail, Acc, KeyAcc);
%% Skip whitespace
compile_attr_key(<<Char, Tail/binary>>, Acc, KeyAcc) when ?IS_SPACE(Char) ->
    compile_attr_key(Tail, Acc, KeyAcc);
compile_attr_key(<<Char, Tail/binary>>, Acc, KeyAcc) ->
    compile_attr_key(Tail, Acc, <<KeyAcc/binary, Char>>);
compile_attr_key(_Tail, _Acc, _KeyAcc) ->
    throw(badarg).

compile_attr_value(<<$', Tail/binary>>, Acc, Key) ->
    compile_attr_quoted_value(Tail, Acc, Key, <<>>);
%% Skip whitespace
compile_attr_value(<<Char, Tail/binary>>, Acc, Key) when ?IS_SPACE(Char) ->
    compile_attr_value(Tail, Acc, Key);
compile_attr_value(<<Char, Tail/binary>>, Acc, Key) ->
    compile_attr_unquoted_value(Tail, Acc, Key, <<Char>>);
compile_attr_value(_Tail, _Acc, _Key) ->
    throw(badarg).

compile_attr_unquoted_value(<<$], Tail/binary>>, Acc, Key, ValueAcc) ->
    compile_path(Tail, [{Key, ValueAcc} | Acc], <<>>);
compile_attr_unquoted_value(<<Char, Tail/binary>>, Acc, Key, ValueAcc) when ?IS_SPACE(Char) ->
    compile_attr_unquoted_value(Tail, Acc, Key, ValueAcc);
compile_attr_unquoted_value(<<Char, Tail/binary>>, Acc, Key, ValueAcc) ->
    compile_attr_unquoted_value(Tail, Acc, Key, <<ValueAcc/binary, Char>>);
compile_attr_unquoted_value(_Tail, _Acc, _Key, _ValueAcc) ->
    throw(badarg).

compile_attr_quoted_value(<<$', Tail/binary>>, Acc, Key, ValueAcc) ->
    compile_attr_end_bracket(Tail, [{Key, ValueAcc} | Acc]);
compile_attr_quoted_value(<<Char, Tail/binary>>, Acc, Key, ValueAcc) ->
    compile_attr_quoted_value(Tail, Acc, Key, <<ValueAcc/binary, Char>>);
compile_attr_quoted_value(_Tail, _Acc, _Key, _ValueAcc) ->
    throw(badarg).

compile_attr_end_bracket(<<$], Tail/binary>>, Acc) ->
    compile_path(Tail, Acc, <<>>);
compile_attr_end_bracket(<<Char, Tail/binary>>, Acc) when ?IS_SPACE(Char) ->
    compile_attr_end_bracket(Tail, Acc);
compile_attr_end_bracket(_Tail, _Acc) ->
    throw(badarg).

compile_index(<<Char, Tail/binary>>, Acc, IndexAcc) when Char >= $0, Char =< $9 ->
    compile_index(Tail, Acc, IndexAcc * 10 + (Char - $0));
compile_index(<<Char, Tail/binary>>, Acc, IndexAcc) when ?IS_SPACE(Char) ->
    compile_index(Tail, Acc, IndexAcc);
compile_index(<<$], Tail/binary>>, Acc, IndexAcc) ->
    compile_path(Tail, [{index, IndexAcc} | Acc], <<>>);
compile_index(_Tail, _Acc, _IndexAcc) ->
    throw(badarg).







