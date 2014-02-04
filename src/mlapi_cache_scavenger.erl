%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011-2012 Juan Jose Comellas
%%% @doc Scavenger process that periodically removes stale entries from the
%%%      cache of MercadoLibre API responses stored in Mnesia.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(mlapi_cache_scavenger).

-behaviour(gen_server).

%% API
-export([start_link/0, scavenge/1, reset_timer/1, expired_keys/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../include/mlapi_cache.hrl").

-define(SERVER, ?MODULE).

-record(state, {
          timers = []
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec scavenge(table()) -> ok.
scavenge(Table) ->
    gen_server:cast(?SERVER, {scavenge, Table}).


-spec reset_timer(table()) -> ok | {error, Reason :: term()}.
reset_timer(Table) ->
    gen_server:call(?SERVER, {reset_timer, Table}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server registering timers to scavenge the cache Mnesia
%%      tables according to each table's expiration time.
-spec init([]) -> {ok, #state{}} | no_return().
init([]) ->
    %% Retrieve the list of cache tables and create a timer for each of them
    %% using the table's time-to-live as the timer interval.
    Metatables = mnesia:dirty_match_object(mlapi_metatable, #mlapi_metatable{_ = '_'}),
    Timers = lists:foldl(fun (#mlapi_metatable{table = Table, time_to_live = TimeToLive}, Acc) ->
                                 %% Create timers to activate the scavenger for each of the tables
                                 get_new_timer(Table, TimeToLive) ++ Acc
                         end, [], Metatables),
    {ok, #state{timers = lists:reverse(Timers)}}.


%% @private
%% @doc Handle call messages.
-spec handle_call(Request :: term(), From :: term(), #state{}) -> {reply, Reply :: term(), #state{}}.
handle_call({reset_timer, Table}, _From, #state{timers = Timers} = _State) ->
    {Response, FinalTimers} = case mnesia:dirty_readread(mlapi_metatable, Table) of
        [#mlapi_metatable{time_to_live = TimeToLive}] ->
            {ok, update_timers(Table, TimeToLive, Timers)};
        [] ->
            {{error, {invalid_table, Table}}, Timers}
    end, 
    {reply, Response, FinalTimers};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% @private
%% @doc Handle cast messages.
-spec handle_cast(Msg :: term(), #state{}) -> {noreply, #state{}}.
handle_cast({scavenge, Table}, State) ->
    lists:foreach(fun (Key) ->
                          mnesia:dirty_delete(Table, Key)
                  end, expired_keys(Table)),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


%% @private
%% @doc Handle all non call/cast messages.
-spec handle_info(Info :: term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.


%% @private
%% @doc This function is called by a gen_server when it is about to
%%      terminate. It should be the opposite of Module:init/1 and do any
%%      necessary cleaning up. When it returns, the gen_server terminates
%%      with Reason. The return value is ignored.
-spec terminate(Reason :: term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.


%% @private
%% @doc Convert process state when code is changed
-spec code_change(OldVsn :: non_neg_integer(), #state{}, Extra :: term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec expired_keys(table()) -> [term()] | no_return().
expired_keys(Table) ->
    Now = mlapi_cache:current_time_in_gregorian_seconds(),
    TimeToLive = time_to_live(Table),
    Guard = {'<', '$2', Now - TimeToLive},
    Result = '$1',
    mnesia:dirty_select(Table, [{#mlapi_cache{key = '$1', last_update = '$2', _ = '_'}, [Guard], [Result]}]).


-spec time_to_live(table()) -> time_to_live().
time_to_live(Table) ->
    case mnesia:dirty_read(mlapi_metatable, Table) of
        [#mlapi_metatable{time_to_live = TimeToLive}] ->
            TimeToLive;
        [] ->
            mlapi_cache:table_time_to_live(Table)
    end.

%% @doc Deletes and recreates the timer for a given table in the timers list
-spec update_timers(table(), time_to_live(), list()) -> list().
update_timers(Table, TimeToLive, Timers) ->
    Timers1 = cancel_old_timer(Table, Timers),
    NewTimers =  get_new_timer(Table, TimeToLive),
    NewTimers ++ Timers1.

%% @doc Removes a timer entry from the list of timers
-spec cancel_old_timer(table(), list()) -> list().
cancel_old_timer(Table, Timers) ->
    case lists:keytake(Table, 1, Timers) of
        {value, {Table, TimerRef}, NewTimers} ->
            timer:cancel(TimerRef),
            NewTimers;
        false ->
            Timers
    end.

%% @doc Create timers to activate the scavenger for the table
-spec get_new_timer(Table::table(), TimeToLive::time_to_live()) -> any().
get_new_timer(Table, TimeToLive) ->
    {ok, TimerRef} = timer:apply_interval(TimeToLive * 1000, ?SERVER, scavenge, [Table]),
    [{Table, TimerRef}].

