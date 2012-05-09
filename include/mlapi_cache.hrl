%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011-2012 Juan Jose Comellas
%%% @doc MercadoLibre API cache type definitions and records.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-ifndef(MLAPI_CACHE_HRL).
-define(MLAPI_CACHE_HRL, "mlapi_cache.hrl").

-type table()           :: atom().
-type table_key()       :: any().
-type table_version()   :: non_neg_integer().
-type timestamp()       :: non_neg_integer().
-type last_update()     :: timestamp().
-type time_to_live()    :: non_neg_integer().

-record(mlapi_metatable, {
          table                                             :: table(),
          version                                           :: table_version(),
          time_to_live                                      :: time_to_live(),    %% in seconds
          last_update                                       :: non_neg_integer(),
          reason                                            :: any()
         }).

-record(mlapi_cache, {
          key                                               :: any(),
          last_update                                       :: last_update(),
          data                                              :: any()
         }).


-endif.
