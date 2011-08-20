%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc Cache of the contents of a MercadoLibre site that is stored in Mnesia.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(mlapi_cache).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([init/0, init/1]).

-include("include/mlapi.hrl").

-type table() :: atom().


-spec init() -> ok | {aborted, Reason :: any()}.
init() ->
    init([node()]).


-spec init([node()]) -> ok | {aborted, Reason :: any()}.
init(Nodes) ->
    try
        mnesia:table_info(mlapi_site, type),
        ok
    catch
        _ : _ ->
            try create_tables(Nodes) of
                {atomic, ok} ->
                    ok
            catch
                _ : Error ->
                    Error
            end
    end.


-spec create_tables([node()]) -> ok | {aborted, Reason :: any()}.
create_tables(Nodes) ->
    lists:foreach(fun (Table) -> create_table(Table, Nodes) end, tables()).


-spec create_table(table(), [node()]) -> {atomic, ok} | {aborted, Reason :: any()}.
create_table(Table, Nodes) ->
    mnesia:create_table(Table, [{access_mode, read_write},
                                {attributes, mlapi:fields(Table)},
                                {disc_copies, [Nodes]},
                                {type, set},
                                {local_content, true}]).



-spec tables() -> [table()].
tables() ->
    [
     mlapi_category,
     mlapi_city,
     mlapi_country,
     mlapi_currency,
     mlapi_listing_exposure,
     mlapi_listing_price,
     mlapi_listing_type,
     mlapi_payment_method,
     mlapi_site,
     mlapi_state
    ].
