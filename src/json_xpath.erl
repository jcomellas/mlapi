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

-export([compile/1, get/2]).

-type key()        :: binary().
-type value()      :: binary() | boolean() | integer() | float() | 'null'.
-type operator()   :: '<' | '=' | '>' | '=<' | '>='.
-type json()       :: binary().
-type ejson()      :: {[{key(), value() | ejson()}]}.
-type path()       :: [key() | {index, non_neg_integer()} | {attr, key()} | {attr, {operator(), key(), value()}}].

-export_type([key/0, value/0, json/0, ejson/0, path/0]).

-define(IS_SPACE(Char), (Char) =:= $\s orelse (Char) =:= $\t orelse (Char) =:= $\r orelse (Char) =:= $\n).


%% @doc
%% Compile an expression using an XPath-like syntax into the format used
%% internally to search for a specific element or group of elements. The syntax
%% is restricted to the following operations:
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
%% @end
-spec compile(Path :: binary() | list()) -> [key() | {index, non_neg_integer()} | {key(), value()}].
compile(<<"//", Tail/binary>>) ->
    {recursive, compile_xpath(Tail, [], <<>>)};
compile(<<"/", Tail/binary>>) ->
    compile_xpath(Tail, [], <<>>);
compile(Path) when is_binary(Path) ->
    {relative, compile_xpath(Path, [], <<>>)};
compile(Path) when is_list(Path) ->
    compile(list_to_binary(Path)).


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

%% Attribute comparisons
compile_attr_key(<<Operator:2/binary, Tail/binary>>, Acc, KeyAcc) when Operator =:= <<"<=">>; Operator =:= <<"!=">>;
                                                                       Operator =:= <<">=">> ->
    compile_attr_value(Tail, Acc, Operator, KeyAcc);
compile_attr_key(<<Operator, Tail/binary>>, Acc, KeyAcc) when Operator =:= $=; Operator =:= $<; Operator =:= $> ->
    compile_attr_value(Tail, Acc, Operator, KeyAcc);
compile_attr_key(<<$], Tail/binary>>, Acc, KeyAcc) ->
    compile_xpath(Tail, [{attr, KeyAcc} | Acc], <<>>);
%% Skip whitespace
compile_attr_key(<<Char, Tail/binary>>, Acc, KeyAcc) when ?IS_SPACE(Char) ->
    compile_attr_key(Tail, Acc, KeyAcc);
compile_attr_key(<<Char, Tail/binary>>, Acc, KeyAcc) ->
    compile_attr_key(Tail, Acc, <<KeyAcc/binary, Char>>);
compile_attr_key(_Tail, _Acc, _KeyAcc) ->
    throw(badarg).

compile_attr_value(<<$', Tail/binary>>, Acc, Operator, Key) ->
    compile_attr_quoted_value(Tail, Acc, Operator, Key, <<>>);
%% Skip whitespace
compile_attr_value(<<Char, Tail/binary>>, Acc, Operator, Key) when ?IS_SPACE(Char) ->
    compile_attr_value(Tail, Acc, Operator, Key);
compile_attr_value(<<Char, Tail/binary>>, Acc, Operator, Key) ->
    compile_attr_unquoted_value(Tail, Acc, Operator, Key, <<Char>>);
compile_attr_value(_Tail, _Acc, _Operator, _Key) ->
    throw(badarg).

compile_attr_unquoted_value(<<$], Tail/binary>>, Acc, Operator, Key, ValueAcc) ->
    compile_xpath(Tail, [{attr, {operator_to_atom(Operator), Key, unquoted_value(ValueAcc)}} | Acc], <<>>);
compile_attr_unquoted_value(<<Char, Tail/binary>>, Acc, Operator, Key, ValueAcc) when ?IS_SPACE(Char) ->
    compile_attr_unquoted_value(Tail, Acc, Operator, Key, ValueAcc);
compile_attr_unquoted_value(<<Char, Tail/binary>>, Acc, Operator, Key, ValueAcc) ->
    compile_attr_unquoted_value(Tail, Acc, Operator, Key, <<ValueAcc/binary, Char>>);
compile_attr_unquoted_value(_Tail, _Acc, _Operator, _Key, _ValueAcc) ->
    throw(badarg).

compile_attr_quoted_value(<<$', Tail/binary>>, Acc, Operator, Key, ValueAcc) ->
    compile_attr_end_bracket(Tail, [{attr, {operator_to_atom(Operator), Key, ValueAcc}} | Acc]);
compile_attr_quoted_value(<<Char, Tail/binary>>, Acc, Operator, Key, ValueAcc) ->
    compile_attr_quoted_value(Tail, Acc, Operator, Key, <<ValueAcc/binary, Char>>);
compile_attr_quoted_value(_Tail, _Acc, _Operator, _Key, _ValueAcc) ->
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



-spec get(binary() | path(), ejson()) -> [ejson() | value()].
get(Path, Json) when is_binary(Path); is_integer(hd(Path)) ->
    get(compile(Path), Json);
get(Path, Json) ->
    case match(Path, Json) of
        undefined ->
            [];
        Element when tuple_size(Element) =:= 1 ->
            [Element];
        Element ->
            Element
    end.


%%match({recursive, Path}, ElementList, _Acc) ->
%%    rec_match(Path, ElementList, []);

%% match([{relative, Path}], ElementList, Acc) ->


match(_Path, undefined) ->
    undefined;
match([Head | Tail], ElementList) when is_list(hd(ElementList)) ->
    NewElementList = lists:foldl(fun (Element, Acc0) ->
                                         case match(Head, Element) of
                                             undefined ->
                                                 Acc0;
                                             ChildElement ->
                                                 [ChildElement | Acc0]
                                         end
                                 end, [], ElementList),
    match(Tail, lists:reverse(NewElementList));
match([{index, Index} | Tail], ElementList) ->
    match(Tail, match_index(Index, ElementList, []));
match([{attr, Expr} | Tail], ElementList) ->
    match(Tail, match_attr(Expr, ElementList, []));
match([Key | Tail], ElementList) when is_list(ElementList) ->
    NewElementList = lists:foldl(fun (Element, Acc0) ->
                                         case match_key(Key, Element) of
                                             undefined ->
                                                 Acc0;
                                             ChildElement ->
                                                 [ChildElement | Acc0]
                                         end
                                 end, [], ElementList),
    match(Tail, lists:reverse(NewElementList));
match([Key | Tail], Element) ->
    match(Tail, match_key(Key, Element));
match([], Element) ->
    Element.



-ifdef(TEST).
rec_match(Path, [Head | Tail] = ElementList, Acc) ->
    case match(Path, ElementList
    Acc = rec_match(Path, Head, Acc0)

rec_match(Path, ElementList, Acc) when is_list(ElementList) ->
    lists:foldl(fun (Element, Acc0) ->
                        rec_match(Path, Element, Acc0)
                end, Acc, ElementList);
rec_match(Path, {[{_Key, Value} | _Tail]} = Element, Acc0) ->
    Acc = case match(Path, Element, []) of
              undefined ->
                  Acc0;
              ChildElementList when is_list(ChildElementList) ->
                  ChildElementList ++ Acc0;
              ChildElement ->
                  [ChildElement | Acc0]
          end,
    rec_match(Path, Value, Acc);
rec_match(_Path, _Element, _Acc) ->
    undefined.
-endif().




match_attr({Operator, Name, Value2} = Expr, [{PropList} = Element | Tail], Acc0) ->
    Acc = case lists:keyfind(Name, 1, PropList) of
              {Name, Value1} ->
                  case compare_attr_value(Operator, Value1, Value2) of
                      true ->
                          [Element | Acc0];
                      false ->
                          Acc0
                  end;
              _  ->
                  Acc0
          end,
    match_attr(Expr, Tail, Acc);
match_attr(Name, [{PropList} = Element | Tail], Acc0) ->
    Acc = case lists:keyfind(Name, 1, PropList) of
              {Name, _Value} ->
                  [Element | Acc0];
              _  ->
                  Acc0
          end,
    match_attr(Name, Tail, Acc);
match_attr(_Expr, [], Acc) ->
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


-spec operator_to_atom(char() | binary()) -> operator().
operator_to_atom($<) -> '<';
operator_to_atom($=) -> '=';
operator_to_atom($>) -> '>';
operator_to_atom(<<"<=">>) -> '=<';
operator_to_atom(<<"!=">>) -> '!=';
operator_to_atom(<<">=">>) -> '>='.


-spec compare_attr_value(operator(), value(), value()) -> boolean().
compare_attr_value('=', Value1, Value2) ->
    Value1 == Value2;
compare_attr_value('<', Value1, Value2) ->
    Value1 < Value2;
compare_attr_value('>', Value1, Value2) ->
    Value1 > Value2;
compare_attr_value('=<', Value1, Value2) ->
    Value1 =< Value2;
compare_attr_value('!=', Value1, Value2) ->
    Value1 /= Value2;
compare_attr_value('>=', Value1, Value2) ->
    Value1 >= Value2.
