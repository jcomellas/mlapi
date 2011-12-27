%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc MercadoLibre API application.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(mlapi_cli).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([exec/2, exec/3, result_to_csv/2, benchmark/2]).

-include("include/mlapi.hrl").


exec([Command | Args], Options) ->
    exec(list_to_existing_atom(Command), Args, Options).


exec(my_orders = Command, _RawArgs, Options) ->
    Args = filter_options(Options, [access_token, feedback_status, limit, offset, payment_status, seller, shipping_status, sort]),
    case mlapi_cache:my_orders(Args, [{format, ejson}]) of
        {error, _Reason} = Error ->
            Error;
        Result ->
            result_to_csv(Command, Result)
    end.


filter_options(Options, ValidNames) ->
    [Option || {Name, _Value} = Option <- Options, lists:member(Name, ValidNames)].


result_to_csv(my_orders, Result) ->
    Headers = [
               <<"ID">>, <<"Estado">>, <<"Fecha Oferta">>,
               <<"Apodo Comprador">>, <<"Email Comprador">>,
               <<"Fecha Calificacion">>, <<"Estado Oferta">>,
               <<"ID Item">>, <<"Titulo">>, <<"Cantidad">>, <<"Precio Unitario">>, <<"Moneda">>
              ],
    Orders = kvc:path(<<"results">>, Result),
    [line_to_csv(Headers) | orders_to_csv(Orders, [])].



orders_to_csv([Order | Tail], Acc) ->
    OrderFieldNames = [<<"id">>, <<"status">>, <<"date_created">>,
                       <<"buyer.nickname">>, <<"buyer.email">>,
                       <<"feedback.sent.date_created">>, <<"feedback.sent.concretion_status">>],
    Line1 = lists:foldl(fun (Path, Acc1) -> [kvc:path(Path, Order) | Acc1] end, [], OrderFieldNames),
    ItemFieldNames = [<<"item.id">>, <<"item.title">>, <<"quantity">>, <<"unit_price">>, <<"currency_id">>],
    Line = case kvc:path(<<"order_items">>, Order) of
               [Item] ->
                   lists:foldl(fun (Path, Acc1) -> [kvc:path(Path, Item) | Acc1] end, Line1, ItemFieldNames);
               [] ->
                   Line1
           end,
    orders_to_csv(Tail, [line_to_csv(lists:reverse(Line)) | Acc]);
orders_to_csv([], Acc) ->
    lists:reverse(Acc).


line_to_csv(Line) ->
    line_to_csv(Line, []).

line_to_csv([Field | Tail], Acc) ->
    line_to_csv(Tail, [$,, $", to_binary(Field), $" | Acc]);
line_to_csv([], [] = Acc) ->
    Acc;
line_to_csv([], Acc) ->
    lists:reverse([$\n | tl(Acc)]).


%% We are only interested in dates (not in times) when exporting to CSV
to_binary({{_Year, _Month, _Day}, {_Hour, _Min, _Sec}} = Datetime) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time_to_local_time(Datetime),
    YYYY = bstr:lpad(bstr:from_integer(Year), 4, $0),
    MM = bstr:lpad(bstr:from_integer(Month), 2, $0),
    DD = bstr:lpad(bstr:from_integer(Day), 2, $0),
    Hh = bstr:lpad(bstr:from_integer(Hour), 2, $0),
    Mm = bstr:lpad(bstr:from_integer(Min), 2, $0),
    Ss = bstr:lpad(bstr:from_integer(Sec), 2, $0),
    <<YYYY/binary, $-, MM/binary, $-, DD/binary, $\s, Hh/binary, $:, Mm/binary, $:, Ss/binary>>;
to_binary(Data) ->
    mlapi:to_binary(Data).



benchmark(Fun, N) ->
    io:format("Benchmarking function (~p time/s) ...~n",[N]),
    statistics(runtime),
    statistics(wall_clock),
    for(1, N, Fun),
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    Sec1 = Time1 / 1000.0,
    Sec2 = Time2 / 1000.0,
    io:format("Erlang elapsed time ~p seconds (runtime) ~p seconds (wall clock)~n",
              [Sec1, Sec2]).


for(N, N, Fun) -> Fun();
for(I, N, Fun) -> Fun(), for(I + 1, N, Fun).
