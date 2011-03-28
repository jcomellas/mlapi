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

-export([get_sites/0, get_site/1,
         get_countries/0, get_country/1,
         get_state/1, get_city/1, get_neighborhood/1,
         get_currencies/0, get_currency/1,
         get_payment_methods/0, get_payment_method/1,
         get_categories/1, get_subcategories/1,
         get_user/1,
         get_item/1,
         get_picture/1,
         search/2, search/4,
         search_category/2, search_category/4,
         search_seller_id/2, search_seller_id/4,
         search_nickname/2, search_nickname/4]).
-export([start/0, stop/0, request/1]).

-type url_path()  :: string().
-type error()     :: {error, Reason :: atom() | {atom(), any()}}.
-type response()  :: {ok, mlapi_json:ejson()} | error().

-export_type([error/0, response/0]).

-define(PROTOCOL, "https").
-define(HOST, "api.mercadolibre.com").
-define(CONTENT_TYPE, "Content-Type").
-define(JSON_MIME_TYPE, "application/json").

-define(SITES,           "/sites").
-define(COUNTRIES,       "/countries").
-define(STATES,          "/states").
-define(CITIES,          "/cities").
-define(NEIGHBORHOODS,   "/neighborhoods").
-define(CURRENCIES,      "/currencies").
-define(PAYMENT_METHODS, "/payment_methods").
-define(CATEGORIES,      "/categories").
-define(USERS,           "/users").
-define(ITEMS,           "/items").
-define(PICTURES,        "/pictures").
-define(SEARCH,          "/search").


start() ->
    application:start(sasl),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(ibrowse),
    application:start(eper),
    application:start(mlapi).


stop() ->
    application:stop(mlapi).


-spec get_sites() -> response().
get_sites() ->
    request(?SITES).

-spec get_site(SiteId :: string() | binary()) -> response().
get_site(SiteId) ->
    request(?SITES "/" ++ to_string(SiteId)).


-spec get_countries() -> response().
get_countries() ->
    request(?COUNTRIES).

-spec get_country(CountryId :: string() | binary()) -> response().
get_country(CountryId) ->
    request(?COUNTRIES "/" ++ to_string(CountryId)).


-spec get_state(StateId :: string() | binary()) -> response().
get_state(StateId) ->
    request(?STATES "/" ++ to_string(StateId)).


-spec get_city(CityId :: string() | binary()) -> response().
get_city(CityId) ->
    request(?CITIES "/" ++ to_string(CityId)).


-spec get_neighborhood(NeighborhoodId :: string() | binary()) -> response().
get_neighborhood(NeighborhoodId) ->
    request(?NEIGHBORHOODS "/" ++ to_string(NeighborhoodId)).


-spec get_currencies() -> response().
get_currencies() ->
    request(?CURRENCIES).

-spec get_currency(CurrencyId :: string() | binary()) -> response().
get_currency(CurrencyId) ->
    request(?CURRENCIES "/" ++ to_string(CurrencyId)).


-spec get_payment_methods() -> response().
get_payment_methods() ->
    request(?PAYMENT_METHODS).

-spec get_payment_method(PaymentMethodId :: string() | binary()) -> response().
get_payment_method(PaymentMethodId) ->
    request(?PAYMENT_METHODS "/" ++ to_string(PaymentMethodId)).


-spec get_categories(SiteId :: string() | binary()) -> {ok, mlapi_json:ejson() | mlapi_json:value()} | error().
get_categories(SiteId) ->
    case get_site(SiteId) of
        {ok, Site} ->
            {ok, mlapi_json:get([<<"categories">>], Site)};
        Error ->
            Error
    end.

-spec get_subcategories(ParentCategoryId :: string() | binary()) -> response().
get_subcategories(ParentCategoryId) ->
    request(?CATEGORIES "/" ++ to_string(ParentCategoryId)).


-spec get_user(UserId :: string() | binary()) -> response().
get_user(UserId) ->
    request(?USERS "/" ++ to_string(UserId)).


-spec get_item(ItemId :: string()) -> response().
get_item(ItemId) ->
    request(?ITEMS "/" ++ ItemId).


-spec get_picture(PictureId :: string()) -> response().
get_picture(PictureId) ->
    request(?PICTURES "/" ++ PictureId).


-spec search(SiteId :: string(), Query :: string()) -> response().
search(SiteId, Query) ->
    request(?SITES "/" ++ SiteId ++ ?SEARCH ++ "?q=" ++ ibrowse_lib:url_encode(Query)).

-spec search(SiteId :: string(), Query :: string(),
             Offset :: non_neg_integer(), Limit :: non_neg_integer()) -> response().
search(SiteId, Query, Offset, Limit) ->
    request(io_lib:format(?SITES "/~s" ?SEARCH "?q=~s&offset=~B&limit=~B",
                          [SiteId, ibrowse_lib:url_encode(Query), Offset, Limit])).


-spec search_category(SiteId :: string(), CategoryId :: string()) -> response().
search_category(SiteId, CategoryId) ->
    request(?SITES "/" ++ SiteId ++ ?SEARCH "?category=" ++ ibrowse_lib:url_encode(CategoryId)).

-spec search_category(SiteId :: string(), CategoryId :: string(),
                      Offset :: non_neg_integer(), Limit :: non_neg_integer()) -> response().
search_category(SiteId, CategoryId, Offset, Limit) ->
    request(io_lib:format(?SITES "/~s" ?SEARCH "?category=~s&offset=~B&limit=~B",
                          [SiteId, ibrowse_lib:url_encode(CategoryId), Offset, Limit])).


-spec search_seller_id(SiteId :: string(), SellerId :: string()) -> response().
search_seller_id(SiteId, SellerId) ->
    request(?SITES "/" ++ SiteId ++ ?SEARCH "?seller_id=" ++ ibrowse_lib:url_encode(SellerId)).

-spec search_seller_id(SiteId :: string(), SellerId :: string(),
                       Offset :: non_neg_integer(), Limit :: non_neg_integer()) -> response().
search_seller_id(SiteId, SellerId, Offset, Limit) ->
    request(io_lib:format(?SITES "/~s" ?SEARCH "?seller_id=~s&offset=~B&limit=~B",
                          [SiteId, ibrowse_lib:url_encode(SellerId), Offset, Limit])).


-spec search_nickname(SiteId :: string(), Nickname :: string()) -> response().
search_nickname(SiteId, Nickname) ->
    request(?SITES "/" ++ SiteId ++ ?SEARCH "?nickname=" ++ ibrowse_lib:url_encode(Nickname)).

-spec search_nickname(SiteId :: string(), Nickname :: string(),
                      Offset :: non_neg_integer(), Limit :: non_neg_integer()) -> response().
search_nickname(SiteId, Nickname, Offset, Limit) ->
    request(io_lib:format(?SITES "/~s" ?SEARCH "?nickname=~s&offset=~B&limit=~B",
                          [SiteId, ibrowse_lib:url_encode(Nickname), Offset, Limit])).


-spec request(url_path()) -> response().
request(Path) ->
    case ibrowse:send_req(?PROTOCOL "://" ?HOST ++ Path, [], get) of
        {ok, "200", Headers, Body} ->
            case lists:keyfind(?CONTENT_TYPE, 1, Headers) of
                {_ContentType, ?JSON_MIME_TYPE ++ _CharSet} ->
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


-spec to_string(string() | binary() | integer() | float() | atom()) -> string().
to_string(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
to_string(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
to_string(Float) when is_float(Float) ->
    float_to_list(Float);
to_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_string(String) ->
    String.
