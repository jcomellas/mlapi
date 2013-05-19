%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011-2012 Juan Jose Comellas
%%% @doc MercadoLibre API application.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(mlapi_app).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(application:start_type(), [term()]) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    ok = ensure_mnesia_schema_created(node()),
    %% Initialize the tables used for the Mnesia cache.
    mlapi_cache:init(mlapi:get_env(cache_nodes, [node()])),
    mlapi_sup:start_link().

stop(_State) ->
    ok.


-spec ensure_mnesia_schema_created(node()) -> ok | {error, Reason :: term()}.
ensure_mnesia_schema_created(Node) ->
    %% This is a "hackish" way of making sure that a Mnesia schema exists on
    %% disk without shutting down Mnesia.
    %% When Mnesia is started without having called mnesia:create_schema/1 in
    %% advance, Mnesia will start with its 'schema' table in RAM and any attempt
    %% to create a table with a 'disc_copies' storage mode will fail. Also,
    %% mnesia:create_schema/1 will fail if called while Mnesia is up.
    %% To solve this problem, we use mnesia:change_table_copy_type/3 to make
    %% the default 'schema' disk-based.
    case mnesia:change_table_copy_type(schema, Node, disc_copies) of
        {atomic, ok} ->
            error_logger:info_msg("Created Mnesia schema for '~s'~n", [Node]),
            ok;
        {aborted, {already_exists, schema, Node, disc_copies}} ->
            ok;
        Error ->
            Error
    end.
