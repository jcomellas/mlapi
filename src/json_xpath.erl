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
-export([compile/1, get/3]).

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
compile(<<"//", Tail/binary>>) ->
    {recursive, compile_xpath(Tail, [], <<>>)};
compile(<<"/", Tail/binary>>) ->
    compile_xpath(Tail, [], <<>>);
compile(Path) ->
    {relative, compile_xpath(Path, [], <<>>)}.


compile_xpath(<<"//", Tail/binary>>, Acc, FieldAcc) ->
    lists:reverse([{recursive, compile_xpath(Tail, [], <<>>)} | add_if_not_empty(FieldAcc, Acc)]);
compile_xpath(<<$/, Tail/binary>>, Acc, FieldAcc) ->
    compile_xpath(Tail, add_if_not_empty(FieldAcc, Acc), <<>>);
compile_xpath(<<$[, Tail/binary>>, Acc, FieldAcc) ->
    compile_array(Tail, add_if_not_empty(FieldAcc, Acc));
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
    compile_xpath(Tail, [{attr, Key, unquoted_value(ValueAcc)} | Acc], <<>>);
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



-ifdef(REC).
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
-endif().


-spec get(binary() | path(), ejson(), Count :: non_neg_integer()) -> [ejson() | value()].
get(Path, Json, Count) when is_binary(Path) ->
    get(compile(Path), Json, Count);
get(Path, Json, Count) ->
    case match(Path, Json, Count, []) of
        undefined ->
            [];
        {_} = Element ->
            [Element];
        Element ->
            Element
    end.


%% match([{relative, Path} | Tail], Json, _Count, Acc) ->

%% match([{recursive, Path} | Tail], Json, _Count, Acc) ->


match(_Path, undefined, _Count, _Acc) ->
    undefined;
match([Head | Tail], ElementList, _Count, Acc) when is_list(hd(ElementList)) ->
    NewElementList = lists:foldl(fun (Element, Acc0) ->
                                         case match(Head, Element, _Count, []) of
                                             undefined ->
                                                 Acc0;
                                             ChildElement ->
                                                 [ChildElement | Acc0]
                                         end
                                 end, [], ElementList),
    match(Tail, lists:reverse(NewElementList), _Count, Acc);
match([{index, Index} | Tail], ElementList, _Count, Acc) ->
    match(Tail, match_index(Index, ElementList, []), _Count, Acc);
match([{attr, Name} | Tail], ElementList, _Count, Acc) ->
    match(Tail, match_attr(Name, ElementList, []), _Count, Acc );
match([{attr, Name, Value} | Tail], ElementList, _Count, Acc) ->
    match(Tail, match_attr(Name, Value, ElementList, []), _Count, Acc);
match([Key | Tail], ElementList, _Count, Acc) when is_list(ElementList) ->
    NewElementList = lists:foldl(fun (Element, Acc0) ->
                                         case match_key(Key, Element) of
                                             undefined ->
                                                 Acc0;
                                             ChildElement ->
                                                 [ChildElement | Acc0]
                                         end
                                 end, [], ElementList),
    match(Tail, lists:reverse(NewElementList), _Count, Acc);
match([Key | Tail], Element, _Count, Acc) ->
    match(Tail, match_key(Key, Element), _Count, Acc);
match([], Element, _Count, _Acc) ->
    Element.

match_attr(Name, [{PropList} = Element | Tail], Acc0) ->
    Acc = case lists:keyfind(Name, 1, PropList) of
              {Name, _Value} ->
                  [Element | Acc0];
              _  ->
                  Acc0
          end,
    match_attr(Name, Tail, Acc);
match_attr(_Name, [], Acc) ->
    lists:reverse(Acc).

match_attr(Name, Value, [{PropList} = Element | Tail], Acc0) ->
    Acc = case lists:keyfind(Name, 1, PropList) of
              {Name, Value} ->
                  [Element | Acc0];
              _  ->
                  Acc0
          end,
    match_attr(Name, Value, Tail, Acc);
match_attr(_Name, _Value, [], Acc) ->
    lists:reverse(Acc).

match_index(Index, [_ | _] = Array, _Acc) ->
    %% Swallow the exception when the index is bigger than the length of the array.
    try lists:nth(Index, Array) of
        Element ->
            Element
    catch
        _:_ ->
            undefined
    end;
match_index(_Index, [], _Acc) ->
    undefined.

match_key(Key, {PropList}) ->
    case lists:keyfind(Key, 1, PropList) of
        {Key, Value} ->
            Value;
        false ->
            undefined
    end;
match_key([], Element) ->
    Element.


unquoted_value(<<"true">>) ->
    true;
unquoted_value(<<"false">>) ->
    false;
unquoted_value(<<"null">>) ->
    null;
unquoted_value(Value) ->
    case is_x(Value, fun is_integer_char/1) of
        true ->
            list_to_integer(binary_to_list(Value));
        false ->
            case is_x(Value, fun is_float_char/1) of
                true ->
                    list_to_float(binary_to_list(Value));
                false ->
                    Value
            end
    end.


-spec is_x(binary(), fun((char()) -> boolean())) -> boolean().
is_x(<<Char, Tail/binary>>, Fun) ->
    case Fun(Char) of
        true ->
            is_x(Tail, Fun);
        false ->
            false
    end;
is_x(<<>>, _Fun) ->
    true.


is_integer_char(Char) ->
    ((Char >= $0) andalso (Char =< $9)) orelse Char =:= $-.


is_float_char(Char) ->
    ((Char >= $0) andalso (Char =< $9)) orelse Char =:= $. orelse Char =:= $-.


add_if_not_empty(<<>>, Acc) ->
    Acc;
add_if_not_empty(Acc0, Acc) ->
    [Acc0 | Acc].
