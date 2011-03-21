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
-module(json_xpath).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([encode/1, decode/1]).
-export([compile/1, get/2]).

-type key() :: binary().
-type value() :: binary() | boolean() | integer() | float() | 'null'.
-type json() :: binary().
-type ejson() :: {[{key(), value() | ejson()}]}.
-type path() :: [key() | {index, non_neg_integer()} | {key(), value()}].

-export_type([key/0, value/0, json/0, ejson/0, path/0]).

-define(IS_SPACE(Char), (Char) =:= $\s orelse (Char) =:= $\t orelse (Char) =:= $\r orelse (Char) =:= $\n).


-spec encode(ejson()) -> {ok, json()}.
encode(EJson) ->
    json:encode(EJson).


-spec decode(json()) -> {ok, ejson()}.
decode(Json) ->
    json:decode(Json).


%% Path is an expression using a syntax similar to that of XPath, but
%% restricted to the following operations:
%%
%% /element
%% /element1/element2/element3
%% /element[index] where is_integer(index)
%% /element[@attr]
%% /element[@attr='value']
%%
%% and the recursive variants:
%%
%% //element
%% //element1/element2/element3
%% //element[index] where is_integer(index)
%% //element[@attr]
%% //element[@attr='value']
%%
%% e.g. <<"/site[@id='MLAPI']/product/component[1]/name">>
-spec compile(Path :: binary()) -> [key() | {index, non_neg_integer()} | {key(), value()}].
compile(<<"//", Path/binary>>) ->
    compile_rec_xpath(Path);
compile(<<"/", Path/binary>>) ->
    compile_abs_xpath(Path);
compile(Path) ->
    compile_rel_xpath(Path).

compile_abs_xpath(Path) ->
    compile_xpath(Path, [], <<>>).

compile_rel_xpath(Path) ->
    {relative, compile_xpath(Path, [], <<>>)}.

compile_rec_xpath(Path) ->
    {recursive, compile_xpath(Path, [], <<>>)}.

compile_xpath(<<$/, Tail/binary>>, Acc, FieldAcc) ->
    compile_xpath(Tail,
                  case FieldAcc of
                      <<>> ->
                          Acc;
                      _ ->
                          [FieldAcc | Acc]
                  end, <<>>);
compile_xpath(<<$[, Tail/binary>>, Acc, FieldAcc) ->
    compile_array(Tail, [FieldAcc | Acc]);
compile_xpath(<<Char, Tail/binary>>, Acc, FieldAcc) ->
    compile_xpath(Tail, Acc, <<FieldAcc/binary, Char>>);
compile_xpath(<<>>, Acc, FieldAcc) when size(FieldAcc) > 0 ->
    lists:reverse([FieldAcc | Acc]);
compile_xpath(<<>>, Acc, _FieldAcc) ->
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
compile_attr_key(<<$], Tail/binary>>, Acc, KeyAcc) ->
    compile_xpath(Tail, [{attr, KeyAcc} | Acc], <<>>);
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
    compile_xpath(Tail, [{attr, Key, ValueAcc} | Acc], <<>>);
compile_attr_unquoted_value(<<Char, Tail/binary>>, Acc, Key, ValueAcc) when ?IS_SPACE(Char) ->
    compile_attr_unquoted_value(Tail, Acc, Key, ValueAcc);
compile_attr_unquoted_value(<<Char, Tail/binary>>, Acc, Key, ValueAcc) ->
    compile_attr_unquoted_value(Tail, Acc, Key, <<ValueAcc/binary, Char>>);
compile_attr_unquoted_value(_Tail, _Acc, _Key, _ValueAcc) ->
    throw(badarg).

compile_attr_quoted_value(<<$', Tail/binary>>, Acc, Key, ValueAcc) ->
    compile_attr_end_bracket(Tail, [{attr, Key, ValueAcc} | Acc]);
compile_attr_quoted_value(<<Char, Tail/binary>>, Acc, Key, ValueAcc) ->
    compile_attr_quoted_value(Tail, Acc, Key, <<ValueAcc/binary, Char>>);
compile_attr_quoted_value(_Tail, _Acc, _Key, _ValueAcc) ->
    throw(badarg).

compile_attr_end_bracket(<<$], Tail/binary>>, Acc) ->
    compile_xpath(Tail, Acc, <<>>);
compile_attr_end_bracket(<<Char, Tail/binary>>, Acc) when ?IS_SPACE(Char) ->
    compile_attr_end_bracket(Tail, Acc);
compile_attr_end_bracket(_Tail, _Acc) ->
    throw(badarg).

compile_index(<<Char, Tail/binary>>, Acc, IndexAcc) when Char >= $0, Char =< $9 ->
    compile_index(Tail, Acc, IndexAcc * 10 + (Char - $0));
compile_index(<<Char, Tail/binary>>, Acc, IndexAcc) when ?IS_SPACE(Char) ->
    compile_index(Tail, Acc, IndexAcc);
compile_index(<<$], Tail/binary>>, Acc, IndexAcc) ->
    compile_xpath(Tail, [{index, IndexAcc} | Acc], <<>>);
compile_index(_Tail, _Acc, _IndexAcc) ->
    throw(badarg).



-spec get(binary() | path(), ejson()) -> ejson() | value().
get([{index, Index} | Tail], Json) when is_integer(Index), is_list(Json) ->
    get(Tail, get_index(Index, Json));
get([{attr, Name} | Tail], Json) when is_list(Json) ->
    get(Tail, get_attr(Name, Json));
get([{attr, Name, Value} | Tail], Json) when is_list(Json) ->
    get(Tail, get_attr(Name, Value, Json));
get([Key | Tail], Json) when is_binary(Key), is_list(Json) ->
    get(Tail, get_key(Key, Json));
get([Key | Tail], Json) ->
    get(Tail, get_key(Key, Json));
get([], Json) ->
    Json;
get(Path, Json) when is_binary(Path) ->
    get(compile(Path), Json).

get_attr(Name, [{Head} = Element | Tail]) ->
    case lists:keyfind(Name, 1, Head) of
        {Name, _Value} ->
            Element;
        _  ->
            get_attr(Name, Tail)
    end;
get_attr(_Name, []) ->
    null.

get_attr(Name, Value, [{Head} = Element | Tail]) ->
    case lists:keyfind(Name, 1, Head) of
        {Name, Value} ->
            Element;
        _  ->
            get_attr(Name, Value, Tail)
    end;
get_attr(_Name, _Value, []) ->
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


-ifdef(REC).
-spec get(binary() | path(), ejson()) -> ejson() | value().
get(Path, Json) ->
    [Value] = get(Path, Json, 1),
    Value.

-spec get(binary() | path(), ejson(), Count :: non_neg_integer()) -> [ejson() | value()].
get(Path, Json, Count) when is_binary(Path) ->
    get(compile_xpath(Path), Json, Count, []).
get(Path, Json, Count) ->
    get(Path, Json, Count, []).

get([{index, Index} | Tail], Json, Count, Acc) when is_integer(Index), is_list(Json) ->
    get(Tail, get_index(Index, Json));
get([{attr, Name} | Tail], Json, Count, Acc) when is_list(Json) ->
    get(Tail, get_attr(Name, Json));
get([{attr, Name, Value} | Tail], Json, Count, Acc) when is_list(Json) ->
    get(Tail, get_attr(Name, Value, Json));
get([Key | Tail], Json, Count, Acc) when is_binary(Key), is_list(Json) ->
    get(Tail, get_key(Key, Json));
get([Key | Tail], Json, Count, Acc) ->
    get(Tail, get_key(Key, Json));
get([], Json, Count, Acc) ->
    Json.

get_attr(Name, [{Head} = Element | Tail]) ->
    case lists:keyfind(Name, 1, Head) of
        {Name, _Value} ->
            Element;
        _  ->
            get_attr(Name, Tail)
    end;
get_attr(_Name, []) ->
    null.

get_attr(Name, Value, [{Head} = Element | Tail]) ->
    case lists:keyfind(Name, 1, Head) of
        {Name, Value} ->
            Element;
        _  ->
            get_attr(Name, Value, Tail)
    end;
get_attr(_Name, _Value, []) ->
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
-endif().



