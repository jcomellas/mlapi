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

-export([exec/1, exec/2, exec/3, usage/1]).
-export([my_orders/2]).
-export([test/0, test/3, benchmark/2, format_headers/2]).

-include("include/mlapi.hrl").

%% Limit imposed my MLAPI when returning paged results.
-define(DEFAULT_OFFSET, 0).
-define(DEFAULT_LIMIT, 50).
-define(DEFAULT_FORMAT, csv).

-type page_position()                           :: first | next | last | undefined.


exec(CmdLine) ->
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, CmdLine) of
        {ok, {Options, NonOptArgs}} ->
            case lists:member(help, Options) of
                true ->
                    usage(OptSpecList, "mlapi_cli");
                false ->
                    io:format("Options:~n  ~p~n~nNon-option arguments:~n  ~p~n", [Options, NonOptArgs]),
                    exec(NonOptArgs, Options)
            end;
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            usage(OptSpecList, "mlapi_cli")
    end.


exec([Command | Args], Options) ->
    exec(list_to_existing_atom(Command), Args, Options).


exec(Command, Args, Options) ->
    erlang:apply(?MODULE, Command, [Args, Options]).


usage(ProgramName) ->
    usage(option_spec_list(), ProgramName).

usage(OptSpecList, ProgramName) ->
    getopt:usage(OptSpecList, ProgramName).



option_spec_list() ->
    [
     %% {Name,         ShortOpt,  LongOpt,           ArgSpec,         HelpMsg}
     {output_file,     $o,        "output-file",     string,          "File where the data will be saved to"},
     {format,          $f,        "format",          {atom, csv},     "Format of the results"},
     {access_token,    $t,        "access-token",    binary,          "MLAPI user access token"},
     {offset,          undefined, "offset",          integer,         "Starting offset for a query that returns the result in pages"},
     {limit,           undefined, "limit",           integer,         "Limit for single pages in a paged result"},
     {sort,            $s,        "sort",            binary,          "Sort order for paged results"},
     {include_headers, $h,        "include-headers", {boolean, true}, "Include headers in paged results"},
     {help,            $?,        "help",            undefined,       "Show the program options"}
    ].


-spec fetch_paged_response(FetchPage :: fun(), FormatLine :: fun(), Options :: list(), Acc :: term()) -> iolist().
fetch_paged_response(FetchPage, FormatLine, Options, Acc) when is_function(FetchPage), is_function(FormatLine) ->
    Offset = proplists:get_value(offset, Options, ?DEFAULT_OFFSET),
    Limit = proplists:get_value(limit, Options, ?DEFAULT_LIMIT),
    {ok, Pager} = mlapi_pager:start_link([{fetch_page, FetchPage}, {offset, Offset}, {limit, Limit}]),
    fetch_pages(Pager, FormatLine, Acc).


-spec fetch_pages(Pager :: pid(), FormatLine :: fun(), Acc0 :: term()) -> iolist().
fetch_pages(Pager, FormatLine, Acc0) ->
    case mlapi_pager:next(Pager) of
        {error, _Reason} = Error ->
            Error;
        %% Position can be one of: first; next; last; undefined.
        {Position, Page} ->
            case kvc:path(<<"results">>, Page) of
                [] ->
                    {error, {results_not_found, Page}};
                Results ->
                    Acc = format_lines(Position, FormatLine, Results, Acc0),
                    case Position of
                        last ->
                            lists:reverse(Acc);
                        _ ->
                            fetch_pages(Pager, FormatLine, Acc)
                    end
            end
    end.


-spec format_lines(page_position(), FormatLine :: fun(), Lines :: list, Acc0 :: term()) -> iolist().
format_lines(Position, FormatLine, [Line | Tail], Acc) ->
    format_lines(Position, FormatLine, Tail, [FormatLine(Position, Line) | Acc]);
format_lines(_Position, _FormatLine, [], Acc) ->
    Acc.



-spec my_orders(list(), list()) -> iolist().
my_orders(_RawArgs, Options) ->
    Format = proplists:get_value(format, Options, ?DEFAULT_FORMAT),
    Args = filter_options(Options, [access_token, feedback_status, payment_status, seller, shipping_status, sort]),
    FetchPage = fun (Offset, Limit) -> mlapi_cache:my_orders([{offset, Offset}, {limit, Limit} | Args], [{format, ejson}]) end,
    FormatLine = fun (_Position, Order) -> format_order(Format, Order) end,
    Headers = [
               <<"ID">>, <<"Fecha Oferta">>,
               <<"Apodo Comprador">>, <<"Nombre Comprador">>, <<"Email Comprador">>, <<"Telefono Comprador">>,
               <<"Fecha Calificacion">>, <<"Estado Calificacion">>,
               <<"ID Item">>, <<"Titulo">>, <<"Cantidad">>, <<"Precio Unitario">>, <<"Moneda">>
              ],
    fetch_paged_response(FetchPage, FormatLine, Options, [format_headers(Format, Headers)]).


format_order(Format, Order) ->
    Buyer = kvc:path(<<"buyer">>, Order),
    FirstName = kvc:path(<<"first_name">>, Buyer),
    LastName = kvc:path(<<"last_name">>, Buyer),
    Phone = case kvc:path(<<"phone.area_code">>, Buyer) of
                AreaCode when is_atom(AreaCode); AreaCode =:= <<>>; AreaCode =:= <<" ">> ->
                    bstr:bstr(kvc:path(<<"phone.number">>, Buyer));
                AreaCode ->
                    Number = bstr:bstr(kvc:path(<<"phone.number">>, Buyer)),
                    <<$(, AreaCode/binary, ") ", Number/binary>>
            end,
    Feedback = kvc:path(<<"feedback.sent">>, Order),
    ReversedLine1 = [
                     kvc:path(<<"concretion_status">>, Feedback),
                     kvc:path(<<"date_created">>, Feedback),
                     Phone,
                     kvc:path(<<"email">>, Buyer),
                     <<FirstName/binary, " ", LastName/binary>>,
                     kvc:path(<<"nickname">>, Buyer),
                     kvc:path(<<"date_created">>, Order),
                     kvc:path(<<"id">>, Order)
                    ],
    ItemFieldNames = [<<"item.id">>, <<"item.title">>, <<"quantity">>, <<"unit_price">>, <<"currency_id">>],
    Line = case kvc:path(<<"order_items">>, Order) of
               [Item] ->
                   lists:foldl(fun (Path, Acc1) -> [kvc:path(Path, Item) | Acc1] end, ReversedLine1, ItemFieldNames);
               [] ->
                   ReversedLine1
           end,
    format_reversed_line(Format, Line).



format_headers(csv, Headers) ->
    line_to_csv(Headers).


%% format_line(csv, Line) ->
%%     line_to_csv(Line).


format_reversed_line(csv, Line) ->
    reversed_line_to_csv(Line).


line_to_csv(Line) ->
    line_to_csv(Line, []).

line_to_csv([Field | Tail], Acc) when is_number(Field); is_boolean(Field) ->
    %% Numbers and booleans don't carry double quotes around them.
    line_to_csv(Tail, [$,, to_binary(Field) | Acc]);
line_to_csv([Field | Tail], Acc) ->
    line_to_csv(Tail, [$,, $", to_binary(Field), $" | Acc]);
line_to_csv([], Acc) ->
    lists:reverse([$\n | tl(Acc)]).


reversed_line_to_csv(Line) ->
    reversed_line_to_csv(Line, [$\n]).

reversed_line_to_csv([Field | Tail], Acc) when is_number(Field); is_boolean(Field) ->
    %% Numbers and booleans don't carry double quotes around them.
    reversed_line_to_csv(Tail, [$,, to_binary(Field) | Acc]);
reversed_line_to_csv([Field | Tail], Acc) ->
    reversed_line_to_csv(Tail, [$,, $", to_binary(Field), $" | Acc]);
reversed_line_to_csv([], Acc) ->
    tl(Acc).


-spec filter_options([tuple()], [atom()]) -> [tuple()].
filter_options(Options, ValidNames) ->
    [Option || {Name, _Value} = Option <- Options, lists:member(Name, ValidNames)].


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


test() ->
    Token = <<"APP_USR-2623-010618-a6869a200067124c996d8d9f11c8f31c-25679280">>,
    test(Token, "my_orders.csv", 0).


test(Token, Filename, Offset) ->
    %% Retrieve the user's ID, get its active sales and export them to a CSV file
    case mlapi_cache:my_user(Token) of
        {error, _Reason} = Error ->
            Error;
        User ->
            SellerId = kvc:path(<<"id">>, User),
            Args = [{access_token, Token}, {seller, SellerId}, {offset, Offset}],
            case mlapi_cli:exec(my_orders, [], Args) of
                {error, _Reason} = Error ->
                    Error;
                Lines ->
                    {ok, File} = file:open(Filename, [write, binary]),
                    lists:foreach(fun (Line) -> file:write(File, Line) end, Lines),
                    file:close(File)
            end
    end.


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








%% exec(my_orders = Command, _RawArgs, Options) ->
%%     Args = filter_options(Options, [access_token, feedback_status, limit, offset, payment_status, seller, shipping_status, sort]),
%%     case mlapi_cache:my_orders(Args, [{format, ejson}]) of
%%         {error, _Reason} = Error ->
%%             Error;
%%         Result ->
%%             result_to_csv(Command, Result)
%%     end.

%% result_to_csv(my_orders, first, Result, Acc) ->
%%     Headers = [
%%                <<"ID">>, <<"Fecha Oferta">>,
%%                <<"Apodo Comprador">>, <<"Email Comprador">>,
%%                <<"Fecha Calificacion">>, <<"Estado Calificacion">>,
%%                <<"ID Item">>, <<"Titulo">>, <<"Cantidad">>, <<"Precio Unitario">>, <<"Moneda">>
%%               ],
%%     result_to_csv(my_orders, next, Result, [Headers | Acc]);
%% result_to_csv(my_orders, next, Result, Acc) ->
%%     Orders = kvc:path(<<"results">>, Result),
%%     [line_to_csv(Headers) | orders_to_csv(Orders, [])].


%% line_spec(my_orders) ->
%%     [{<<"id">>,                                 <<"ID">>,                    10, left}
%%      {<<"date_created">>,                       <<"Fecha Oferta">>,          10, left},
%%      {<<"buyer.nickname">>,                     <<"Apodo Comprador">>,       20, left},
%%      {<<"buyer.email">>,                        <<"Email Comprador">>,       30, left},
%%      {<<"feedback.sent.date_created">>,         <<"Fecha Calificacion">>,    30, left},
%%      {<<"feedback.sent.concretion_status">>,    <<"Estado Calificacion">>,   14, left},
%%      {<<"item.id">>,                            <<"ID Item">>,               12, left},
%%      {<<"item.title">>, <<"quantity">>, <<"unit_price">>, <<"currency_id">>}].

%% orders_to_csv([Order | Tail], Acc) ->
%%     OrderFieldNames = [<<"id">>, <<"date_created">>,
%%                        <<"buyer.nickname">>, <<"buyer.email">>,
%%                        <<"feedback.sent.date_created">>, <<"feedback.sent.concretion_status">>],
%%     Line1 = lists:foldl(fun (Path, Acc1) -> [kvc:path(Path, Order) | Acc1] end, [], OrderFieldNames),
%%     ItemFieldNames = [<<"item.id">>, <<"item.title">>, <<"quantity">>, <<"unit_price">>, <<"currency_id">>],
%%     Line = case kvc:path(<<"order_items">>, Order) of
%%                [Item] ->
%%                    lists:foldl(fun (Path, Acc1) -> [kvc:path(Path, Item) | Acc1] end, Line1, ItemFieldNames);
%%                [] ->
%%                    Line1
%%            end,
%%     orders_to_csv(Tail, [line_to_csv(lists:reverse(Line)) | Acc]);
%% orders_to_csv([], Acc) ->
%%     lists:reverse(Acc).
