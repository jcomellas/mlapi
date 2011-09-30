%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc MercadoLibre API.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(mlapi).
-author('Juan Jose Comellas <juanjo@comellas.org>').

%%-export([query/2, query_category/2, query_seller_id/2, query_seller_nick]).

-export([start/0, stop/0, request/1, get_env/0, get_env/1, get_env/2]).
-export([get_sites/0, get_sites/1, get_site/1, get_site/2,
         get_countries/0, get_countries/1, get_country/1, get_country/2,
         get_state/1, get_state/2, get_city/1, get_city/2,
         get_currencies/0, get_currencies/1, get_currency/1, get_currency/2,
         get_currency_conversion/2, get_currency_conversion/3, get_currency_conversion/4,
         get_listing_exposures/1, get_listing_exposures/2, get_listing_exposure/2, get_listing_exposure/3,
         get_listing_types/1, get_listing_types/2, get_listing_prices/1, get_listing_prices/2,
         get_payment_types/0, get_payment_types/1, get_payment_type/1, get_payment_type/2,
         get_payment_methods/1, get_payment_methods/2, get_payment_method/2, get_payment_method/3,
         get_card_issuers/1, get_card_issuers/2, get_card_issuer/2, get_card_issuer/3,
         get_category/1, get_category/2,
         get_user/1, get_user/2,
         get_item/1, get_item/2,
         get_picture/1, get_picture/2,
         get_trends/1, get_trends/2, get_category_trends/2, get_category_trends/3, get_category_trends/4,
         get_local_geolocation/0, get_local_geolocation/1, get_geolocation/1, get_geolocation/2,
         search/2, search/3, search/4, search/5,
         search_category/2, search_category/3, search_category/4, search_category/5,
         search_seller_id/2, search_seller_id/3, search_seller_id/4, search_seller_id/5,
         search_seller_nick/2, search_seller_nick/3, search_seller_nick/4, search_seller_nick/5]).
-export([json_to_record/2, json_to_proplist/2, json_to_orddict/2, json_to_term/3,
         json_field_to_record_name/2,
         is_json_datetime_field/2, iso_datetime_to_tuple/1]).
-export([site_to_country/1, country_to_site/1]).

-include("include/mlapi.hrl").
-compile({parse_transform, dynarec}).

-type url_path()          :: string().
-type error()             :: {error, Reason :: atom() | {atom(), any()}}.
-type ejson_key()         :: binary().
-type ejson_value()       :: binary() | boolean() | integer() | float() | 'null'.
-type ejson()             :: {[{ejson_key(), ejson_value() | ejson()}]}.
-type proplist()          :: [proplists:property()].
-type format()            :: 'binary' | 'json' | 'proplist' | 'orddict' | 'record'.
-type option()            :: {format, format()} | {record, RecordName :: atom()} | 'refresh'.
-type response()          :: binary() | ejson() | proplist() | orddict:orddict() | tuple() | error().


-record(json_helper, {
          child_to_term  :: fun(),
          append         :: fun(),
          finish         :: fun()
         }).

-export_type([url_path/0, ejson/0, option/0, format/0, response/0, error/0]).

-define(APP, mlapi).
-define(PROTOCOL, "https").
-define(HOST, "api.mercadolibre.com").
-define(HEADER_CONTENT_TYPE, "Content-Type").
-define(MIME_TYPE_JSON, "application/json").

-define(SITES,                "/sites").
-define(COUNTRIES,            "/countries").
-define(STATES,               "/states").
-define(CITIES,               "/cities").
-define(NEIGHBORHOODS,        "/neighborhoods").
-define(CURRENCIES,           "/currencies").
-define(CURRENCY_CONVERSIONS, "/currency_conversions/search").
-define(LISTING_EXPOSURES,    "/listing_exposures").
-define(LISTING_TYPES,        "/listing_types").
-define(LISTING_PRICES,       "/listing_prices").
-define(PAYMENT_TYPES,        "/payment_types").
-define(PAYMENT_METHODS,      "/payment_methods").
-define(CARD_ISSUERS,         "/card_issuers").
-define(CATEGORIES,           "/categories").
-define(USERS,                "/users").
-define(ITEMS,                "/items").
-define(PICTURES,             "/pictures").
-define(SEARCH,               "/search").
-define(TRENDS,               "/trends/search").
-define(GEOLOCATION,          "/geolocation").

-define(SET_RECORD(RecordName, Options), (lists:keystore(record, 1, Options, {record, RecordName}))).


%% @doc Start the application and all its dependencies.
start() ->
    application:start(sasl),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(ibrowse),
    application:start(mnesia),
    %% application:start(eper),
    application:start(mlapi).


%% @doc Stop the application.
stop() ->
    application:stop(mlapi).


%% @doc Retrieve all key/value pairs in the env for the specified app.
-spec get_env() -> [{Key :: atom(), Value :: term()}].
get_env() ->
    application:get_all_env(?APP).

%% @doc The official way to get a value from the app's env.
%%      Will return the 'undefined' atom if that key is unset.
-spec get_env(Key :: atom()) -> term().
get_env(Key) ->
    get_env(Key, undefined).

%% @doc The official way to get a value from this application's env.
%%      Will return Default if that key is unset.
-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    case application:get_env(?APP, Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.


-spec get_sites() -> response().
get_sites() ->
    get_sites([]).

-spec get_sites([option()]) -> response().
get_sites(Options) ->
    request(?SITES, ?SET_RECORD(mlapi_site, Options)).

-spec get_site(mlapi_site_id() | string()) -> response().
get_site(SiteId) ->
    get_site(SiteId, []).

-spec get_site(mlapi_site_id() | string(), [option()]) -> response().
get_site(SiteId, Options) ->
    request(?SITES "/" ++ to_string(SiteId), ?SET_RECORD(mlapi_site_ext, Options)).


-spec get_countries() -> response().
get_countries() ->
    get_countries([]).

-spec get_countries([option()]) -> response().
get_countries(Options) ->
    request(?COUNTRIES, ?SET_RECORD(mlapi_country, Options)).

-spec get_country(mlapi_country_id() | string()) -> response().
get_country(CountryId) ->
    get_country(CountryId, []).

-spec get_country(mlapi_country_id() | string(), [option()]) -> response().
get_country(CountryId, Options) ->
    request(?COUNTRIES "/" ++ to_string(CountryId), ?SET_RECORD(mlapi_country_ext, Options)).


-spec get_state(mlapi_state_id() | string()) -> response().
get_state(StateId) ->
    get_state(StateId, []).

-spec get_state(mlapi_state_id() | string(), [option()]) -> response().
get_state(StateId, Options) ->
    request(?STATES "/" ++ to_string(StateId), ?SET_RECORD(mlapi_state_ext, Options)).


-spec get_city(mlapi_city_id() | string()) -> response().
get_city(CityId) ->
    get_city(CityId, []).

-spec get_city(mlapi_city_id() | string(), [option()]) -> response().
get_city(CityId, Options) ->
    request(?CITIES "/" ++ to_string(CityId), ?SET_RECORD(mlapi_city_ext, Options)).


-spec get_currencies() -> response().
get_currencies() ->
    get_currencies([]).

-spec get_currencies([option()]) -> response().
get_currencies(Options) ->
    request(?CURRENCIES, ?SET_RECORD(mlapi_currency, Options)).

-spec get_currency(mlapi_currency_id() | string()) -> response().
get_currency(CurrencyId) ->
    get_currency(CurrencyId, []).

-spec get_currency(mlapi_currency_id() | string(), [option()]) -> response().
get_currency(CurrencyId, Options) ->
    request(?CURRENCIES "/" ++ to_string(CurrencyId), ?SET_RECORD(mlapi_currency_ext, Options)).


-spec get_currency_conversion(FromCurrencyId :: mlapi_currency_id() | string(),
                              ToCurrencyId :: mlapi_currency_id() | string()) -> response().
get_currency_conversion(FromCurrencyId, ToCurrencyId) ->
    get_currency_conversion(FromCurrencyId, ToCurrencyId, []).

-spec get_currency_conversion(FromCurrencyId :: mlapi_currency_id() | string(),
                              ToCurrencyId :: mlapi_currency_id() | string(), [option()] | calendar:datetime()) -> response().
get_currency_conversion(FromCurrencyId, ToCurrencyId, Options) when is_list(Options) ->
    request(?CURRENCY_CONVERSIONS "?from=" ++ to_string(FromCurrencyId) ++ "&to=" ++ to_string(ToCurrencyId),
            ?SET_RECORD(mlapi_currency_conversion, Options));
get_currency_conversion(FromCurrencyId, ToCurrencyId, DateTime) ->
    get_currency_conversion(FromCurrencyId, ToCurrencyId, DateTime, []).

-spec get_currency_conversion(FromCurrencyId :: mlapi_currency_id() | string(),
                              ToCurrencyId :: mlapi_currency_id() | string(), calendar:datetime(), [option()]) -> response().
get_currency_conversion(FromCurrencyId, ToCurrencyId, {{Year, Month, Day}, {Hour, Min, _Sec}}, Options) ->
    %% The conversion date must be formatted as: dd/MM/yyyy-HH:mm
    DateArg = io_lib:format("&date=~2.2.0w/~2.2.0w/~4.4.0w-~2.2.0w:~2.2.0w", [Day, Month, Year, Hour, Min]),
    request(?CURRENCY_CONVERSIONS "?from=" ++ to_string(FromCurrencyId) ++ "&to=" ++ to_string(ToCurrencyId) ++ DateArg,
            ?SET_RECORD(mlapi_currency_conversion, Options)).


-spec get_listing_exposures(mlapi_site_id() | string()) -> response().
get_listing_exposures(SiteId) ->
    get_listing_exposures(SiteId, []).

-spec get_listing_exposures(mlapi_site_id() | string(), [option()]) -> response().
get_listing_exposures(SiteId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?LISTING_EXPOSURES, ?SET_RECORD(mlapi_listing_exposures, Options)).

-spec get_listing_exposure(mlapi_site_id() | string(), mlapi_listing_exposure_id() | string()) -> response().
get_listing_exposure(SiteId, ListingExposureId) ->
    get_listing_exposure(SiteId, ListingExposureId, []).

-spec get_listing_exposure(mlapi_site_id() | string(), mlapi_listing_exposure_id() | string(), [option()]) -> response().
get_listing_exposure(SiteId, ListingExposureId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?LISTING_EXPOSURES "/" ++ to_string(ListingExposureId),
            ?SET_RECORD(mlapi_listing_exposure, Options)).


-spec get_listing_types(mlapi_site_id() | string()) -> response().
get_listing_types(SiteId) ->
    get_listing_types(SiteId, []).

-spec get_listing_types(mlapi_site_id() | string(), [option()]) -> response().
get_listing_types(SiteId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?LISTING_TYPES, ?SET_RECORD(mlapi_listing_type, Options)).


-spec get_listing_prices(mlapi_site_id() | string()) -> response().
get_listing_prices(SiteId) ->
    get_listing_prices(SiteId, []).

-spec get_listing_prices(mlapi_site_id() | string(), [option()]) -> response().
get_listing_prices(SiteId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?LISTING_PRICES "?price=1", ?SET_RECORD(mlapi_listing_price, Options)).


-spec get_payment_types() -> response().
get_payment_types() ->
    get_payment_types([]).

-spec get_payment_types([option()]) -> response().
get_payment_types(Options) ->
    request(?PAYMENT_TYPES, ?SET_RECORD(mlapi_payment_type, Options)).

-spec get_payment_type(mlapi_payment_type_id() | string()) -> response().
get_payment_type(PaymentTypeId) ->
    get_payment_type(PaymentTypeId, []).

-spec get_payment_type(mlapi_payment_type_id() | string(), [option()]) -> response().
get_payment_type(PaymentTypeId, Options) ->
    request(?PAYMENT_TYPES "/" ++ to_string(PaymentTypeId), ?SET_RECORD(mlapi_payment_type, Options)).


-spec get_payment_methods(mlapi_site_id() | string()) -> response().
get_payment_methods(SiteId) ->
    get_payment_methods(SiteId, []).

-spec get_payment_methods(mlapi_site_id() | string(), [option()]) -> response().
get_payment_methods(SiteId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?PAYMENT_METHODS, ?SET_RECORD(mlapi_payment_method, Options)).

-spec get_payment_method(mlapi_site_id() | string(), mlapi_payment_method_id() | string()) -> response().
get_payment_method(SiteId, PaymentMethodId) ->
    get_payment_method(SiteId, PaymentMethodId, []).

-spec get_payment_method(mlapi_site_id() | string(), mlapi_payment_method_id() | string(), [option()]) -> response().
get_payment_method(SiteId, PaymentMethodId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?PAYMENT_METHODS "/" ++ to_string(PaymentMethodId),
            ?SET_RECORD(mlapi_payment_method_ext, Options)).


-spec get_card_issuers(mlapi_site_id() | string()) -> response().
get_card_issuers(SiteId) ->
    get_card_issuers(SiteId, []).

-spec get_card_issuers(mlapi_site_id() | string(), [option()]) -> response().
get_card_issuers(SiteId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?CARD_ISSUERS, ?SET_RECORD(mlapi_card_issuer, Options)).

-spec get_card_issuer(mlapi_site_id() | string(), mlapi_card_issuer_id() | string()) -> response().
get_card_issuer(SiteId, CardIssuerId) ->
    get_card_issuer(SiteId, CardIssuerId, []).

-spec get_card_issuer(mlapi_site_id() | string(), mlapi_card_issuer_id() | string(), [option()]) -> response().
get_card_issuer(SiteId, CardIssuerId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?CARD_ISSUERS "/" ++ to_string(CardIssuerId),
            ?SET_RECORD(mlapi_card_issuer_ext, Options)).


-spec get_category(mlapi_category_id() | string()) -> response().
get_category(CategoryId) ->
    get_category(CategoryId, []).

-spec get_category(mlapi_category_id() | string(), [option()]) -> response().
get_category(CategoryId, Options) ->
    request(?CATEGORIES "/" ++ to_string(CategoryId), ?SET_RECORD(mlapi_category_ext, Options)).


-spec get_user(mlapi_user_id() | string()) -> response().
get_user(UserId) ->
    get_user(UserId, []).

-spec get_user(mlapi_user_id() | string(), [option()]) -> response().
get_user(UserId, Options) ->
    request(?USERS "/" ++ to_string(UserId), ?SET_RECORD(mlapi_user, Options)).


-spec get_item(mlapi_item_id() | string()) -> response().
get_item(ItemId) ->
    get_item(ItemId, []).

-spec get_item(mlapi_item_id() | string(), [option()]) -> response().
get_item(ItemId, Options) ->
    request(?ITEMS "/" ++ to_string(ItemId), ?SET_RECORD(mlapi_item, Options)).


-spec get_picture(mlapi_picture_id() | string()) -> response().
get_picture(PictureId) ->
    get_picture(PictureId, []).

-spec get_picture(mlapi_picture_id() | string(), [option()]) -> response().
get_picture(PictureId, Options) ->
    request(?PICTURES "/" ++ to_string(PictureId), ?SET_RECORD(mlapi_picture, Options)).


-spec get_trends(mlapi_site_id() | string()) -> response().
get_trends(SiteId) ->
    get_trends(SiteId, []).

-spec get_trends(mlapi_site_id() | string(), [option()]) -> response().
get_trends(SiteId, Options) when is_list(Options) ->
    request(?TRENDS "?site=" ++ to_string(SiteId), ?SET_RECORD(mlapi_trend, Options)).


-spec get_category_trends(mlapi_site_id() | string(), mlapi_category_id() | string()) -> response().
get_category_trends(SiteId, CategoryId) ->
    get_category_trends(SiteId, CategoryId, []).

-spec get_category_trends(mlapi_site_id() | string(), mlapi_category_id() | string(), [option()] | non_neg_integer()) -> response().
get_category_trends(SiteId, CategoryId, Options) when is_list(Options) ->
    request(?TRENDS "?site=" ++ to_string(SiteId) ++ "&category=" ++ to_string(CategoryId),
            ?SET_RECORD(mlapi_trend, Options));
get_category_trends(SiteId, CategoryId, Limit) ->
    get_category_trends(SiteId, CategoryId, Limit, []).

-spec get_category_trends(mlapi_site_id() | string(), mlapi_category_id() | string(), Limit :: non_neg_integer(), [option()]) -> response().
get_category_trends(SiteId, CategoryId, Limit, Options) ->
    request(?TRENDS "?site=" ++ to_string(SiteId) ++ "&category=" ++ to_string(CategoryId) ++ io_lib:format("&limit=~w", [Limit]),
            ?SET_RECORD(mlapi_trend, Options)).


-spec get_local_geolocation() -> response().
get_local_geolocation() ->
    get_local_geolocation([]).

-spec get_local_geolocation([option()]) -> response().
get_local_geolocation(Options) ->
    request(?GEOLOCATION "/whereami", ?SET_RECORD(mlapi_geolocation, Options)).

-spec get_geolocation(mlapi_ip_address() | string()) -> response().
get_geolocation(IpAddr) ->
    get_geolocation(IpAddr, []).

-spec get_geolocation(mlapi_ip_address() | string(), [option()]) -> response().
get_geolocation(IpAddr, Options) ->
    request(?GEOLOCATION "/ip/" ++ to_string(IpAddr), ?SET_RECORD(mlapi_geolocation, Options)).


-spec search(mlapi_site_id() | string(), Query :: string()) -> response().
search(SiteId, Query) ->
    search(SiteId, Query, []).

-spec search(mlapi_site_id() | string(), Query :: string(), [option()]) -> response().
search(SiteId, Query, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?SEARCH ++ "?q=" ++ ibrowse_lib:url_encode(to_string(Query)),
            ?SET_RECORD(mlapi_search_result, Options)).

-spec search(mlapi_site_id() | string(), Query :: string(),
             Offset :: non_neg_integer(), Limit :: non_neg_integer()) -> response().
search(SiteId, Query, Offset, Limit) ->
    search(SiteId, Query, Offset, Limit, []).

-spec search(mlapi_site_id() | string(), Query :: string(),
             Offset :: non_neg_integer(), Limit :: non_neg_integer(), [option()]) -> response().
search(SiteId, Query, Offset, Limit, Options) ->
    request(io_lib:format(?SITES "/~s" ?SEARCH "?q=~s&offset=~w&limit=~w",
                          [SiteId, ibrowse_lib:url_encode(to_string(Query)), Offset, Limit]),
            ?SET_RECORD(mlapi_search_result, Options)).


-spec search_category(mlapi_site_id() | string(), mlapi_category_id() | string()) -> response().
search_category(SiteId, CategoryId) ->
    search_category(SiteId, CategoryId, []).

-spec search_category(mlapi_site_id() | string(), mlapi_category_id() | string(), [option()]) -> response().
search_category(SiteId, CategoryId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?SEARCH "?category=" ++ ibrowse_lib:url_encode(to_string(CategoryId)),
            ?SET_RECORD(mlapi_search_result, Options)).

-spec search_category(mlapi_site_id() | string(), mlapi_category_id() | string(),
                      Offset :: non_neg_integer(), Limit :: non_neg_integer()) -> response().
search_category(SiteId, CategoryId, Offset, Limit) ->
    search_category(SiteId, CategoryId, Offset, Limit, []).

-spec search_category(mlapi_site_id() | string(), mlapi_category_id() | string(),
                      Offset :: non_neg_integer(), Limit :: non_neg_integer(), [option()]) -> response().
search_category(SiteId, CategoryId, Offset, Limit, Options) ->
    request(io_lib:format(?SITES "/~s" ?SEARCH "?category=~s&offset=~w&limit=~w",
                          [SiteId, ibrowse_lib:url_encode(to_string(CategoryId)), Offset, Limit]),
            ?SET_RECORD(mlapi_search_result, Options)).


-spec search_seller_id(mlapi_site_id() | string(), mlapi_user_id() | string()) -> response().
search_seller_id(SiteId, SellerId) ->
    search_seller_id(SiteId, SellerId).

-spec search_seller_id(mlapi_site_id() | string(), mlapi_user_id() | string(), [option()]) -> response().
search_seller_id(SiteId, SellerId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?SEARCH "?seller_id=" ++ ibrowse_lib:url_encode(to_string(SellerId)),
            ?SET_RECORD(mlapi_search_result, Options)).

-spec search_seller_id(mlapi_site_id() | string(), mlapi_user_id() | string(),
                       Offset :: non_neg_integer(), Limit :: non_neg_integer()) -> response().
search_seller_id(SiteId, SellerId, Offset, Limit) ->
    search_seller_id(SiteId, SellerId, Offset, Limit, []).

-spec search_seller_id(mlapi_site_id() | string(), mlapi_user_id() | string(),
                       Offset :: non_neg_integer(), Limit :: non_neg_integer(), [option()]) -> response().
search_seller_id(SiteId, SellerId, Offset, Limit, Options) ->
    request(io_lib:format(?SITES "/~s" ?SEARCH "?seller_id=~s&offset=~w&limit=~w",
                          [SiteId, ibrowse_lib:url_encode(to_string(SellerId)), Offset, Limit]),
            ?SET_RECORD(mlapi_search_result, Options)).


-spec search_seller_nick(mlapi_site_id() | string(), Nickname :: mlapi_user_name() | string()) -> response().
search_seller_nick(SiteId, Nickname) ->
    search_seller_nick(SiteId, Nickname, []).

-spec search_seller_nick(mlapi_site_id() | string(), Nickname :: mlapi_user_name() | string(), [option()]) -> response().
search_seller_nick(SiteId, Nickname, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?SEARCH "?nickname=" ++ ibrowse_lib:url_encode(to_string(Nickname)),
            ?SET_RECORD(mlapi_search_result, Options)).

-spec search_seller_nick(mlapi_site_id() | string(), Nickname :: mlapi_user_name() | string(),
                         Offset :: non_neg_integer(), Limit :: non_neg_integer()) -> response().
search_seller_nick(SiteId, Nickname, Offset, Limit) ->
    search_seller_nick(SiteId, Nickname, Offset, Limit, []).

-spec search_seller_nick(mlapi_site_id() | string(), Nickname :: mlapi_user_name() | string(),
                         Offset :: non_neg_integer(), Limit :: non_neg_integer(), [option()]) -> response().
search_seller_nick(SiteId, Nickname, Offset, Limit, Options) ->
    request(io_lib:format(?SITES "/~s" ?SEARCH "?nickname=~s&offset=~w&limit=~w",
                          [SiteId, ibrowse_lib:url_encode(to_string(Nickname)), Offset, Limit]),
            ?SET_RECORD(mlapi_search_result, Options)).


-spec request(url_path()) -> response().
request(Path) ->
    request(Path, []).

-spec request(url_path(), [option()]) -> response().
request(Path, Options) ->
    case ibrowse:send_req(get_env(protocol, ?PROTOCOL) ++ "://" ++ get_env(host, ?HOST) ++ Path, [], get) of
        {ok, "200", Headers, Body} ->
            case lists:keyfind(?HEADER_CONTENT_TYPE, 1, Headers) of
                {_ContentType, ?MIME_TYPE_JSON ++ _CharSet} ->
                    case proplists:get_value(format, Options, json) of
                        binary ->
                            Body;
                        Format ->
                            try
                                json_to_term(ejson:decode(Body), proplists:get_value(record, Options), Format)
                            catch
                                throw:Reason ->
                                    {error, Reason}
                            end
                    end;
                InvalidContentType ->
                    {error, {invalid_content_type, InvalidContentType}}
            end;
        {ok, Code, _Headers, _Body} ->
            {error, response_reason(Code)};

        {error, _Reason} = Error ->
            Error
    end.


-spec json_to_term(ejson(), RecordName :: atom(), format()) -> ejson() | orddict:orddict() | proplist() | tuple().
json_to_term(Doc, _RecordName, json) ->
    Doc;
json_to_term(Doc, RecordName, orddict) ->
    json_to_orddict(Doc, RecordName);
json_to_term(Doc, RecordName, proplist) ->
    json_to_proplist(Doc, RecordName);
json_to_term(Doc, RecordName, record) ->
    json_to_record(Doc, RecordName).


%% @doc Convert a JSON document or a list of documents into one or more known record.
-spec json_to_record(tuple() | [tuple()], Record :: atom() | tuple()) -> tuple() | [tuple()].
json_to_record({Elements}, RecordOrName) when is_list(Elements) ->
    JsonHelperFun = #json_helper{
      child_to_term = fun json_to_record/2,
      append = fun (Name, Value, Record) -> set_value(Name, Value, Record) end,
      finish = fun (Record) -> Record end
     },
    {RecordName, Record} = if
                               is_tuple(RecordOrName) ->
                                   {element(1, RecordOrName), RecordOrName};
                               is_atom(RecordOrName) ->
                                   {RecordOrName, new_record(RecordOrName)}
                           end,
    json_list_to_term(RecordName, JsonHelperFun, Elements, Record);
json_to_record(Elements, RecordName) when is_list(Elements) ->
    lists:reverse(
      lists:foldl(fun (Element, Acc) ->
                          [json_to_record(Element, new_record(RecordName)) | Acc]
                  end, [], Elements)).


%% @doc Convert a JSON document or a list of documents into one or more property lists.
-spec json_to_proplist(tuple() | [tuple()], RecordName :: atom()) -> proplist() | [proplist()].
json_to_proplist({Elements}, RecordName) when is_list(Elements) ->
    JsonHelperFun = #json_helper{
      child_to_term = fun json_to_proplist/2,
      append = fun (Name, Value, Acc) -> [{Name, Value} | Acc] end,
      finish = fun lists:reverse/1
     },
    json_list_to_term(RecordName, JsonHelperFun, Elements, []);
json_to_proplist(Elements, RecordName) when is_list(Elements) ->
    lists:reverse(
      lists:foldl(fun (Element, Acc) ->
                          [json_to_proplist(Element, RecordName) | Acc]
                  end, [], Elements)).


%% @doc Convert a JSON document or a list of documents into one or more ordered dictionaries.
-spec json_to_orddict(tuple() | [tuple()], RecordName :: atom()) -> orddict:orddict() | [orddict:orddict()].
json_to_orddict({Elements}, RecordName) when is_list(Elements) ->
    JsonHelperFun = #json_helper{
      child_to_term = fun json_to_orddict/2,
      append = fun orddict:append/3,
      finish = fun (Dict) -> Dict end
     },
    json_list_to_term(RecordName, JsonHelperFun, Elements, orddict:new());
json_to_orddict(Elements, RecordName) when is_list(Elements) ->
    lists:reverse(
      lists:foldl(fun (Element, Acc) ->
                          [json_to_orddict(Element, RecordName) | Acc]
                  end, [], Elements)).

-spec json_list_to_term(RecordName :: atom(), #json_helper{}, [{binary(), any()}], tuple() | orddict:orddict() | proplist()) ->
                               tuple() | orddict:orddict() | proplist().
json_list_to_term(RecordName, JsonHelperFun, [{Name, Value} | Tail], Acc) ->
    FieldName = binary_to_existing_atom(Name, utf8),
    %% Convert the value to a record if possible
    NewValue =
        case json_field_to_record_name(RecordName, FieldName) of
            undefined ->
                case is_json_datetime_field(RecordName, FieldName) of
                    true ->
                        iso_datetime_to_tuple(Value);
                    false ->
                        Value
                end;
            ChildRecordName ->
                if
                    is_tuple(Value) orelse is_list(Value) ->
                        (JsonHelperFun#json_helper.child_to_term)(Value, ChildRecordName);
                    true ->
                        Value
                end
        end,
    json_list_to_term(RecordName, JsonHelperFun, Tail, (JsonHelperFun#json_helper.append)(FieldName, NewValue, Acc));
json_list_to_term(_RecordName, JsonHelperFun, [], Acc) ->
    (JsonHelperFun#json_helper.finish)(Acc).


%% @doc Return the record name for those JSON fields that can be converted to a known child record.
-spec json_field_to_record_name(ParentRecordName :: atom(), FieldName :: atom()) -> ChildRecordName :: atom() | undefined.
json_field_to_record_name(mlapi_buyer_reputation, transactions) ->
    mlapi_transactions;
json_field_to_record_name(mlapi_category_ext, children_categories) ->
    mlapi_category;
json_field_to_record_name(mlapi_category_ext, settings) ->
    mlapi_settings;
json_field_to_record_name(mlapi_country_ext, states) ->
    mlapi_state;
json_field_to_record_name(mlapi_exceptions_by_card_issuer, card_issuer) ->
    mlapi_card_issuer;
json_field_to_record_name(mlapi_exceptions_by_card_issuer, payer_costs) ->
    mlapi_payer_costs;
json_field_to_record_name(mlapi_filter, values) ->
    mlapi_filter_value;
json_field_to_record_name(mlapi_geo_information, location) ->
    mlapi_location;
json_field_to_record_name(mlapi_item, attributes) ->
    mlapi_attribute;
json_field_to_record_name(mlapi_item, city) ->
    mlapi_city;
json_field_to_record_name(mlapi_item, country) ->
    mlapi_country;
json_field_to_record_name(mlapi_item, descriptions) ->
    mlapi_description;
json_field_to_record_name(mlapi_item, geolocation) ->
    mlapi_location;
json_field_to_record_name(mlapi_payment_method_ext, exceptions_by_card_issuer) ->
    mlapi_exceptions_by_card_issuer;
json_field_to_record_name(mlapi_item, pictures) ->
    mlapi_picture;
json_field_to_record_name(mlapi_item, seller_address) ->
    mlapi_seller_address;
json_field_to_record_name(mlapi_item, shipping) ->
    mlapi_shipping;
json_field_to_record_name(mlapi_item, state) ->
    mlapi_state;
json_field_to_record_name(mlapi_seller_reputation, transactions) ->
    mlapi_transactions;
json_field_to_record_name(mlapi_search_item, address) ->
    mlapi_search_address;
json_field_to_record_name(mlapi_search_item, attributes) ->
    mlapi_attribute;
json_field_to_record_name(mlapi_search_item, seller) ->
    mlapi_seller;
json_field_to_record_name(mlapi_search_item, installments) ->
    mlapi_installment;
json_field_to_record_name(mlapi_site_ext, categories) ->
    mlapi_category;
json_field_to_record_name(mlapi_site_ext, currencies) ->
    mlapi_currency;
json_field_to_record_name(mlapi_state_ext, cities) ->
    mlapi_city;
json_field_to_record_name(mlapi_search_result, filters) ->
    mlapi_filter;
json_field_to_record_name(mlapi_search_result, available_filters) ->
    mlapi_filter;
json_field_to_record_name(mlapi_search_result, paging) ->
    mlapi_paging;
json_field_to_record_name(mlapi_search_result, results) ->
    mlapi_search_item;
json_field_to_record_name(mlapi_search_result, seller) ->
    mlapi_seller;
json_field_to_record_name(mlapi_search_result, sort) ->
    mlapi_sort;
json_field_to_record_name(mlapi_search_result, available_sorts) ->
    mlapi_sort;
json_field_to_record_name(mlapi_user, identification) ->
    mlapi_identification;
json_field_to_record_name(mlapi_user, buyer_reputation) ->
    mlapi_buyer_reputation;
json_field_to_record_name(mlapi_user, phone) ->
    mlapi_phone;
json_field_to_record_name(mlapi_user, seller_reputation) ->
    mlapi_seller_reputation;
json_field_to_record_name(mlapi_user, status) ->
    mlapi_status;
json_field_to_record_name(_RecordName, geo_information) ->
    mlapi_geo_information;
json_field_to_record_name(_RecordName, _FieldName) ->
    undefined.


%% @doc Check whether a field of a record should be converted to a datetime.
-spec is_json_datetime_field(RecordName :: atom(), FieldName :: atom()) -> boolean().
is_json_datetime_field(mlapi_shipping_costs, time) ->
    true;
is_json_datetime_field(mlapi_item, start_time) ->
    true;
is_json_datetime_field(mlapi_item, stop_time) ->
    true;
is_json_datetime_field(mlapi_search_item, stop_time) ->
    true;
is_json_datetime_field(mlapi_user, registration_date) ->
    true;
is_json_datetime_field(_RecordName, _FieldName) ->
    false.


%% @doc Convert a datetime in the ISO format to a UTC-based datetime tuple.
-spec iso_datetime_to_tuple(binary()) -> calendar:datetime() | binary().
iso_datetime_to_tuple(<<Year:4/binary, $-, Month:2/binary, $-, Day:2/binary, $T,
                        Hour:2/binary, $:, Min:2/binary, $:, Sec:2/binary, $., _Millisec:3/binary, $Z>>) ->
    {{bstr:to_integer(Year), bstr:to_integer(Month), bstr:to_integer(Day)},
     {bstr:to_integer(Hour), bstr:to_integer(Min), bstr:to_integer(Sec)}};
iso_datetime_to_tuple(<<Year:4/binary, $-, Month:2/binary, $-, Day:2/binary, $T,
                        Hour:2/binary, $:, Min:2/binary, $:, Sec:2/binary, $., _Millisec:3/binary, Sign,
                        TimezoneHour:2/binary, $:, TimezoneMin:2/binary>>) ->
    LocalSecs = calendar:datetime_to_gregorian_seconds({{bstr:to_integer(Year), bstr:to_integer(Month), bstr:to_integer(Day)},
                                                        {bstr:to_integer(Hour), bstr:to_integer(Min), bstr:to_integer(Sec)}}),
    %% Convert the the seconds in the local timezone to UTC.
    UtcSecs = case ((bstr:to_integer(TimezoneHour) * 60 + bstr:to_integer(TimezoneMin)) * 60) of
                  Offset when Sign =:= $- ->
                      LocalSecs - Offset;
                  Offset ->
                      LocalSecs + Offset
              end,
    calendar:gregorian_seconds_to_datetime(UtcSecs);
iso_datetime_to_tuple(<<>>) ->
    undefined;
iso_datetime_to_tuple(Value) ->
    Value.


%% @doc Convert an HTTP response code to its corresponding reason.
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


-spec site_to_country(SiteId :: mlapi_site_id()) -> mlapi_country_id().
site_to_country(SiteId) ->
    case lists:keyfind(SiteId, 1, site_country_map()) of
        {SiteId, CountryId} ->
            CountryId;
        _ ->
            undefined
    end.


-spec country_to_site(CountryId :: mlapi_country_id()) -> mlapi_site_id().
country_to_site(CountryId) ->
    case lists:keyfind(CountryId, 2, site_country_map()) of
        {SiteId, CountryId} ->
            SiteId;
        _ ->
            undefined
    end.


-spec site_country_map() -> [{mlapi_site_id(), mlapi_country_id()}].
site_country_map() ->
    [
     {<<"MLA">>, <<"AR">>},  %% Argentina
     {<<"MLB">>, <<"BR">>},  %% Brasil
     {<<"MCO">>, <<"CO">>},  %% Colombia
     {<<"MCR">>, <<"CR">>},  %% Costa Rica
     {<<"MEC">>, <<"EC">>},  %% Ecuador
     {<<"MLC">>, <<"CL">>},  %% Chile
     {<<"MLM">>, <<"MX">>},  %% Mexico
     {<<"MLU">>, <<"UY">>},  %% Uruguay
     {<<"MLV">>, <<"VE">>},  %% Venezuela
     {<<"MPA">>, <<"PA">>},  %% Panamá
     {<<"MPE">>, <<"PE">>},  %% Perú
     {<<"MPT">>, <<"PT">>},  %% Portugal
     {<<"MRD">>, <<"DO">>}   %% República Dominicana
    ].


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
