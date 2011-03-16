%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2009 Juan Jose Comellas
%%% @doc Module that performs searches on the MercadoLibre API.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(mlapi).
-author('Juan Jose Comellas <juanjo@comellas.org>').

%%-export([query/2, query_category/2, query_seller_id/2, query_seller_nick]).

-export([get_sites/0, get_countries/0, get_item/1]).
-export([start/0, stop/0, request/1]).

-define(PROTOCOL, "https://").
-define(HOST, "api.mercadolibre.com").
-define(CONTENT_TYPE, "Content-Type").

-define(SITES, "/sites").
-define(COUNTRIES, "/countries").
-define(STATES, "/states").
-define(CURRENCIES, "/currencies").
-define(PAYMENT_METHODS, "/payment_methods").
-define(USERS, "/users").
-define(ITEMS, "/items").

%% -type url() :: string().
-type url_path() :: string().
-type key() :: binary().
-type value() :: any().
-type proplist() :: [{key(), value() | proplist()}].


start() ->
    application:start(sasl),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(ibrowse),
    application:start(mlapi).


stop() ->
    application:stop(mlapi).


get_sites() ->
    request(?SITES).


get_countries() ->
    request(?COUNTRIES).

get_item(Item) ->
    request(?ITEMS ++ "/" ++ Item).


-spec request(url_path()) -> {ok, proplist()} | {error, Reason :: any()}.
request(Path) ->
    case ibrowse:send_req(?PROTOCOL ++ ?HOST ++ Path, [], get) of
        {ok, "200", Headers, Body} ->
            case lists:keyfind(?CONTENT_TYPE, 1, Headers) of
                {_ContentType, "application/json" ++ _CharSet} ->
                    json:decode(Body);
                InvalidContentType ->
                    {error, {invalid_content_type, InvalidContentType}}
            end;
        {ok, Code, _Headers, _Body} ->
            {error, response_reason(Code)};

        {error, _Reason} = Error ->
            Error
    end.


-spec response_reason(string()) -> atom().
response_reason("100") -> continue;
response_reason("101") -> switching_protocols;
response_reason("200") -> ok;
response_reason("201") -> created;
response_reason("202") -> accepted;
response_reason("203") -> non_authoritative_information;
response_reason("204") -> no_content;
response_reason("205") -> reset_content;
response_reason("206") -> partial_content;
response_reason("300") -> multiple_choices;
response_reason("301") -> moved_permanently;
response_reason("302") -> found;
response_reason("303") -> see_other;
response_reason("304") -> not_modified;
response_reason("305") -> use_proxy;
response_reason("307") -> temporary_redirect;
response_reason("400") -> bad_request;
response_reason("401") -> unauthorized;
response_reason("402") -> payment_required;
response_reason("403") -> forbidden;
response_reason("404") -> not_found;
response_reason("405") -> method_not_allowed;
response_reason("406") -> not_acceptable;
response_reason("407") -> proxy_authentication_required;
response_reason("408") -> request_timeout;
response_reason("409") -> conflict;
response_reason("410") -> gone;
response_reason("411") -> length_required;
response_reason("412") -> precondition_failed;
response_reason("413") -> request_entity_too_large;
response_reason("414") -> request_uri_too_large;
response_reason("415") -> unsupported_media_type;
response_reason("416") -> requested_range_not_satisfiable;
response_reason("417") -> expectation_failed;
response_reason("500") -> internal_server_error;
response_reason("501") -> not_implemented;
response_reason("502") -> bad_gateway;
response_reason("503") -> service_unavailable;
response_reason("504") -> gateway_timeout;
response_reason("505") -> http_version_not_supported;
response_reason(Code) -> Code.
