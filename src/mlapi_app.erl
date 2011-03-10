-module(mlapi_app).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    mlapi_sup:start_link().

stop(_State) ->
    ok.
