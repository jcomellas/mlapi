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

-export([start/0, stop/0, request/1, request/2, get_env/0, get_env/1, get_env/2]).
%% Public APIs
-export([applications/0, applications/1, application/1, application/2,
         sites/0, sites/1, site/1, site/2,
         countries/0, countries/1, country/1, country/2,
         state/1, state/2, city/1, city/2,
         currencies/0, currencies/1, currency/1, currency/2,
         currency_conversion/2, currency_conversion/3, currency_conversion/4,
         listing_exposures/1, listing_exposures/2, listing_exposure/2, listing_exposure/3,
         listing_types/1, listing_types/2,
         listing_prices/2, listing_prices/3,
         payment_types/0, payment_types/1, payment_type/1, payment_type/2,
         payment_methods/1, payment_methods/2, payment_method/2, payment_method/3,
         card_issuers/1, card_issuers/2, card_issuer/2, card_issuer/3,
         credit_level/1, credit_level/2,
         category/1, category/2,
         domains/0, domains/1, domain/1, domain/2,
         user/1, user/2,
         item/1, item/2,
         picture/1, picture/2,
         question/1, question/2, questions/1, questions/2,
         trends/1, trends/2,
         local_geolocation/0, local_geolocation/1, geolocation/1, geolocation/2,
         search/2, search/3
        ]).
%% Private APIs
-export([current_user/1, current_user/2,
         user_listing_types/2, user_listing_types/3,
         user_items/2, user_items/3
        ]).
%% post_question/2, delete_question/2, post_answer/2, hide_questions/2,

-export([ejson_to_record/2, ejson_to_proplist/2, ejson_to_orddict/2, ejson_to_term/3,
         ejson_field_to_record_name/2,
         is_ejson_datetime_field/2, iso_datetime_to_tuple/1]).
-export([site_to_country/1, country_to_site/1]).

-include("include/mlapi.hrl").
-compile({parse_transform, dynarec}).

-type url_path()          :: string().
-type error()             :: {error, Reason :: atom() | {atom(), any()}}.
-type ejson_key()         :: binary().
-type ejson_value()       :: binary() | boolean() | integer() | float() | 'null'.
-type ejson()             :: {[{ejson_key(), ejson_value() | ejson()}]}.
-type proplist()          :: [proplists:property()].
-type format()            :: 'binary' | 'ejson' | 'proplist' | 'orddict' | 'record'.
-type option()            :: {format, format()} | {record, RecordName :: atom()} | 'refresh'.
-type response()          :: binary() | ejson() | proplist() | orddict:orddict() | tuple() | error().


-export_type([url_path/0, ejson/0, option/0, format/0, response/0, error/0]).

-define(APP, mlapi).
-define(PROTOCOL, "https").
-define(HOST, "api.mercadolibre.com").
-define(HEADER_CONTENT_TYPE, "Content-Type").
-define(MIME_TYPE_JSON, "application/json").

-define(APPLICATIONS,            "/applications").
-define(SITES,                   "/sites").
-define(COUNTRIES,               "/countries").
-define(STATES,                  "/states").
-define(CITIES,                  "/cities").
-define(NEIGHBORHOODS,           "/neighborhoods").
-define(CURRENCIES,              "/currencies").
-define(CURRENCY_CONVERSIONS,    "/currency_conversions/search").
-define(LISTING_EXPOSURES,       "/listing_exposures").
-define(AVAILABLE_LISTING_TYPES, "/available_listing_types").
-define(LISTING_TYPES,           "/listing_types").
-define(LISTING_PRICES,          "/listing_prices").
-define(PAYMENT_TYPES,           "/payment_types").
-define(PAYMENT_METHODS,         "/payment_methods").
-define(CARD_ISSUERS,            "/card_issuers").
-define(CATEGORIES,              "/categories").
-define(DOMAINS,                 "/domains").
-define(CREDIT_LEVELS,           "/credit_levels").
-define(USERS,                   "/users").
-define(ITEMS,                   "/items").
-define(PICTURES,                "/pictures").
-define(QUESTIONS,               "/questions").
-define(SEARCH,                  "/search").
-define(TRENDS,                  "/trends/search").
-define(GEOLOCATION,             "/geolocation").

-define(SET_RECORD(RecordName, Options), (lists:keystore(record, 1, Options, {record, RecordName}))).

-record(state, {
          access_token,
          expiration_time
         }).

-record(json_helper, {
          child_to_term  :: fun(),
          append         :: fun(),
          finish         :: fun()
         }).


%% @doc Start the application and all its dependencies.
start() ->
    application:start(sasl),
    application:start(mnesia),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(ibrowse),
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


-spec applications() -> response().
applications() ->
    applications([]).

-spec applications([option()]) -> response().
applications(Options) ->
    request(?APPLICATIONS, ?SET_RECORD(mlapi_application, Options)).

-spec application(mlapi_application_id()) -> response().
application(ApplicationId) ->
    application(ApplicationId, []).

-spec application(mlapi_application_id(), [option()]) -> response().
application(ApplicationId, Options) ->
    request(?APPLICATIONS "/" ++ to_string(ApplicationId), ?SET_RECORD(mlapi_application, Options)).


-spec sites() -> response().
sites() ->
    sites([]).

-spec sites([option()]) -> response().
sites(Options) ->
    request(?SITES, ?SET_RECORD(mlapi_site, Options)).

-spec site(mlapi_site_id()) -> response().
site(SiteId) ->
    site(SiteId, []).

-spec site(mlapi_site_id(), [option()]) -> response().
site(SiteId, Options) ->
    request(?SITES "/" ++ to_string(SiteId), ?SET_RECORD(mlapi_site_ext, Options)).


-spec countries() -> response().
countries() ->
    countries([]).

-spec countries([option()]) -> response().
countries(Options) ->
    request(?COUNTRIES, ?SET_RECORD(mlapi_country, Options)).

-spec country(mlapi_country_id()) -> response().
country(CountryId) ->
    country(CountryId, []).

-spec country(mlapi_country_id(), [option()]) -> response().
country(CountryId, Options) ->
    request(?COUNTRIES "/" ++ to_string(CountryId), ?SET_RECORD(mlapi_country_ext, Options)).


-spec state(mlapi_state_id()) -> response().
state(StateId) ->
    state(StateId, []).

-spec state(mlapi_state_id(), [option()]) -> response().
state(StateId, Options) ->
    request(?STATES "/" ++ to_string(StateId), ?SET_RECORD(mlapi_state_ext, Options)).


-spec city(mlapi_city_id()) -> response().
city(CityId) ->
    city(CityId, []).

-spec city(mlapi_city_id(), [option()]) -> response().
city(CityId, Options) ->
    request(?CITIES "/" ++ to_string(CityId), ?SET_RECORD(mlapi_city_ext, Options)).


-spec currencies() -> response().
currencies() ->
    currencies([]).

-spec currencies([option()]) -> response().
currencies(Options) ->
    request(?CURRENCIES, ?SET_RECORD(mlapi_currency, Options)).

-spec currency(mlapi_currency_id()) -> response().
currency(CurrencyId) ->
    currency(CurrencyId, []).

-spec currency(mlapi_currency_id(), [option()]) -> response().
currency(CurrencyId, Options) ->
    request(?CURRENCIES "/" ++ to_string(CurrencyId), ?SET_RECORD(mlapi_currency_ext, Options)).


-spec currency_conversion(FromCurrencyId :: mlapi_currency_id(), ToCurrencyId :: mlapi_currency_id()) -> response().
currency_conversion(FromCurrencyId, ToCurrencyId) ->
    currency_conversion(FromCurrencyId, ToCurrencyId, []).

-spec currency_conversion(FromCurrencyId :: mlapi_currency_id(), ToCurrencyId :: mlapi_currency_id(),
                          [option()] | calendar:datetime()) -> response().
currency_conversion(FromCurrencyId, ToCurrencyId, Options) when is_list(Options) ->
    request(?CURRENCY_CONVERSIONS "?from=" ++ to_string(FromCurrencyId) ++ "&to=" ++ to_string(ToCurrencyId),
            ?SET_RECORD(mlapi_currency_conversion, Options));
currency_conversion(FromCurrencyId, ToCurrencyId, DateTime) ->
    currency_conversion(FromCurrencyId, ToCurrencyId, DateTime, []).

-spec currency_conversion(FromCurrencyId :: mlapi_currency_id(), ToCurrencyId :: mlapi_currency_id(),
                          calendar:datetime(), [option()]) -> response().
currency_conversion(FromCurrencyId, ToCurrencyId, {{Year, Month, Day}, {Hour, Min, _Sec}}, Options) ->
    %% The conversion date must be formatted as: dd/MM/yyyy-HH:mm
    DateArg = io_lib:format("&date=~2.2.0w/~2.2.0w/~4.4.0w-~2.2.0w:~2.2.0w", [Day, Month, Year, Hour, Min]),
    request(?CURRENCY_CONVERSIONS "?from=" ++ to_string(FromCurrencyId) ++ "&to=" ++ to_string(ToCurrencyId) ++ DateArg,
            ?SET_RECORD(mlapi_currency_conversion, Options)).


-spec listing_exposures(mlapi_site_id()) -> response().
listing_exposures(SiteId) ->
    listing_exposures(SiteId, []).

-spec listing_exposures(mlapi_site_id(), [option()]) -> response().
listing_exposures(SiteId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?LISTING_EXPOSURES, ?SET_RECORD(mlapi_listing_exposures, Options)).

-spec listing_exposure(mlapi_site_id(), mlapi_listing_exposure_id()) -> response().
listing_exposure(SiteId, ListingExposureId) ->
    listing_exposure(SiteId, ListingExposureId, []).

-spec listing_exposure(mlapi_site_id(), mlapi_listing_exposure_id(), [option()]) -> response().
listing_exposure(SiteId, ListingExposureId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?LISTING_EXPOSURES "/" ++ to_string(ListingExposureId),
            ?SET_RECORD(mlapi_listing_exposure, Options)).


-spec listing_types(mlapi_site_id()) -> response().
listing_types(SiteId) ->
    listing_types(SiteId, []).

-spec listing_types(mlapi_site_id(), [option()]) -> response().
listing_types(SiteId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?LISTING_TYPES, ?SET_RECORD(mlapi_listing_type, Options)).


-spec listing_prices(mlapi_site_id(), mlapi_listing_price_filter()) -> response().
listing_prices(SiteId, Filter) ->
    listing_prices(SiteId, Filter, []).

-spec listing_prices(mlapi_site_id(), mlapi_listing_price_filter(), [option()]) -> response().
listing_prices(SiteId, Filter, Options) ->
    FilterExpr = string:join(listing_prices_filter(Filter, []), "&"),
    request(?SITES "/" ++ to_string(SiteId) ++ ?LISTING_PRICES "?" ++ FilterExpr, ?SET_RECORD(mlapi_listing_price, Options)).

listing_prices_filter([{price, Price} | Tail], Acc) ->
    listing_prices_filter(Tail, ["price=" ++ to_string(Price) | Acc]);
listing_prices_filter([{listing_type_id, ListingTypeId} | Tail], Acc) ->
    listing_prices_filter(Tail, ["listing_type_id=" ++ to_string(ListingTypeId) | Acc]);
listing_prices_filter([{quantity, Quantity} | Tail], Acc) ->
    listing_prices_filter(Tail, ["quantity=" ++ to_string(Quantity) | Acc]);
listing_prices_filter([{category_id, CategoryId} | Tail], Acc) ->
    listing_prices_filter(Tail, ["category_id=" ++ to_string(CategoryId) | Acc]);
listing_prices_filter([{currency_id, CurrencyId} | Tail], Acc) ->
    listing_prices_filter(Tail, ["cy_id=" ++ to_string(CurrencyId) | Acc]);
listing_prices_filter([], Acc) ->
    lists:reverse(Acc).


-spec payment_types() -> response().
payment_types() ->
    payment_types([]).

-spec payment_types([option()]) -> response().
payment_types(Options) ->
    request(?PAYMENT_TYPES, ?SET_RECORD(mlapi_payment_type, Options)).

-spec payment_type(mlapi_payment_type_id()) -> response().
payment_type(PaymentTypeId) ->
    payment_type(PaymentTypeId, []).

-spec payment_type(mlapi_payment_type_id(), [option()]) -> response().
payment_type(PaymentTypeId, Options) ->
    request(?PAYMENT_TYPES "/" ++ to_string(PaymentTypeId), ?SET_RECORD(mlapi_payment_type, Options)).


-spec payment_methods(mlapi_site_id()) -> response().
payment_methods(SiteId) ->
    payment_methods(SiteId, []).

-spec payment_methods(mlapi_site_id(), [option()]) -> response().
payment_methods(SiteId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?PAYMENT_METHODS, ?SET_RECORD(mlapi_payment_method, Options)).

-spec payment_method(mlapi_site_id(), mlapi_payment_method_id()) -> response().
payment_method(SiteId, PaymentMethodId) ->
    payment_method(SiteId, PaymentMethodId, []).

-spec payment_method(mlapi_site_id(), mlapi_payment_method_id(), [option()]) -> response().
payment_method(SiteId, PaymentMethodId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?PAYMENT_METHODS "/" ++ to_string(PaymentMethodId),
            ?SET_RECORD(mlapi_payment_method_ext, Options)).


-spec card_issuers(mlapi_site_id()) -> response().
card_issuers(SiteId) ->
    card_issuers(SiteId, []).

-spec card_issuers(mlapi_site_id(), [option()]) -> response().
card_issuers(SiteId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?CARD_ISSUERS, ?SET_RECORD(mlapi_card_issuer, Options)).

-spec card_issuer(mlapi_site_id(), mlapi_card_issuer_id()) -> response().
card_issuer(SiteId, CardIssuerId) ->
    card_issuer(SiteId, CardIssuerId, []).

-spec card_issuer(mlapi_site_id(), mlapi_card_issuer_id(), [option()]) -> response().
card_issuer(SiteId, CardIssuerId, Options) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?CARD_ISSUERS "/" ++ to_string(CardIssuerId),
            ?SET_RECORD(mlapi_card_issuer_ext, Options)).


-spec credit_level(mlapi_credit_level_id()) -> response().
credit_level(CreditLevelId) ->
    credit_level(CreditLevelId, []).

-spec credit_level(mlapi_credit_level_id(), [option()]) -> response().
credit_level(CreditLevelId, Options) ->
    request(?USERS ?CREDIT_LEVELS "/" ++ to_string(CreditLevelId), ?SET_RECORD(mlapi_credit_level, Options)).


-spec category(mlapi_category_id()) -> response().
category(CategoryId) ->
    category(CategoryId, []).

-spec category(mlapi_category_id(), [option()]) -> response().
category(CategoryId, Options) ->
    request(?CATEGORIES "/" ++ to_string(CategoryId), ?SET_RECORD(mlapi_category_ext, Options)).


-spec domains() -> response().
domains() ->
    domains([]).

-spec domains([option()]) -> response().
domains(Options) ->
    request(?DOMAINS, ?SET_RECORD(mlapi_domain, Options)).


-spec domain(mlapi_domain_id()) -> response().
domain(DomainId) ->
    domain(DomainId, []).

-spec domain(mlapi_domain_id(), [option()]) -> response().
domain(DomainId, Options) ->
    request(?DOMAINS "/" ++ to_string(DomainId), ?SET_RECORD(mlapi_domain, Options)).


-spec user(mlapi_user_id()) -> response().
user(UserId) ->
    user(UserId, []).

-spec user(mlapi_user_id(), [option()]) -> response().
user(UserId, Options) ->
    request(?USERS "/" ++ to_string(UserId), ?SET_RECORD(mlapi_user, Options)).


-spec item(mlapi_item_id()) -> response().
item(ItemId) ->
    item(ItemId, []).

-spec item(mlapi_item_id(), [option()]) -> response().
item(ItemId, Options) ->
    request(?ITEMS "/" ++ to_string(ItemId), ?SET_RECORD(mlapi_item, Options)).


-spec picture(mlapi_picture_id()) -> response().
picture(PictureId) ->
    picture(PictureId, []).

-spec picture(mlapi_picture_id(), [option()]) -> response().
picture(PictureId, Options) ->
    request(?PICTURES "/" ++ to_string(PictureId), ?SET_RECORD(mlapi_picture, Options)).


-spec question(mlapi_question_id()) -> response().
question(QuestionId) ->
    question(QuestionId, []).

-spec question(mlapi_question_id(), [option()]) -> response().
question(QuestionId, Options) ->
    request(?QUESTIONS "/" ++ to_string(QuestionId), ?SET_RECORD(mlapi_question, Options)).


-spec questions([mlapi_question_filter()]) -> response().
questions(Filter) ->
    questions(Filter, []).

-spec questions([mlapi_question_filter()], [option()]) -> response().
questions(Filter, Options) ->
    FilterExpr = string:join(questions_filter(Filter, []), "&"),
    request(?QUESTIONS "/search?" ++ FilterExpr, ?SET_RECORD(mlapi_question_result, Options)).

questions_filter([{access_token, AccessToken} | Tail], Acc) ->
    questions_filter(Tail, ["access_token=" ++ to_string(AccessToken) | Acc]);
questions_filter([{item, ItemId} | Tail], Acc) ->
    questions_filter(Tail, ["item=" ++ to_string(ItemId) | Acc]);
questions_filter([{period, Period} | Tail], Acc) ->
    questions_filter(Tail, ["period=" ++ to_string(Period) | Acc]);
questions_filter([{status, Status} | Tail], Acc) ->
    questions_filter(Tail, ["status=" ++ to_string(Status) | Acc]);
questions_filter([{from, UserId} | Tail], Acc) ->
    questions_filter(Tail, ["from=" ++ to_string(UserId) | Acc]);
questions_filter([{seller, UserId} | Tail], Acc) ->
    questions_filter(Tail, ["seller=" ++ to_string(UserId) | Acc]);
questions_filter([], Acc) ->
    lists:reverse(Acc).


-spec trends(mlapi_trend_filter()) -> response().
trends(Filter) ->
    trends(Filter, []).

-spec trends(mlapi_trend_filter(), [option()]) -> response().
trends(Filter, Options) when is_list(Filter), is_list(Options) ->
    FilterExpr = string:join(trends_filter(Filter, []), "&"),
    request(?TRENDS "?" ++ FilterExpr, ?SET_RECORD(mlapi_trend, Options)).

trends_filter([{site, SiteId} | Tail], Acc) ->
    trends_filter(Tail, ["site=" ++ to_string(SiteId) | Acc]);
trends_filter([{category, CategoryId} | Tail], Acc) ->
    trends_filter(Tail, ["category=" ++ to_string(CategoryId) | Acc]);
trends_filter([{limit, Limit} | Tail], Acc) ->
    trends_filter(Tail, ["limit=" ++ to_string(Limit) | Acc]);
trends_filter([], Acc) ->
    lists:reverse(Acc).


-spec local_geolocation() -> response().
local_geolocation() ->
    local_geolocation([]).

-spec local_geolocation([option()]) -> response().
local_geolocation(Options) ->
    request(?GEOLOCATION "/whereami", ?SET_RECORD(mlapi_geolocation, Options)).

-spec geolocation(mlapi_ip_address()) -> response().
geolocation(IpAddr) ->
    geolocation(IpAddr, []).

-spec geolocation(mlapi_ip_address(), [option()]) -> response().
geolocation(IpAddr, Options) ->
    request(?GEOLOCATION "/ip/" ++ to_string(IpAddr), ?SET_RECORD(mlapi_geolocation, Options)).


-spec search(mlapi_site_id(), mlapi_search_filter()) -> response().
search(SiteId, Filter) ->
    search(SiteId, Filter, []).

-spec search(mlapi_site_id(), mlapi_search_filter(), [option()]) -> response().
search(SiteId, Filter, Options) ->
    FilterExpr = string:join(search_filter(Filter, []), "&"),
    request(?SITES "/" ++ to_string(SiteId) ++ ?SEARCH "?" ++ FilterExpr, ?SET_RECORD(mlapi_search_result, Options)).

search_filter([{nickname, Nickname} | Tail], Acc) ->
    search_filter(Tail, ["nickname=" ++ url_encode(Nickname) | Acc]);
search_filter([{seller_id, SellerId} | Tail], Acc) ->
    search_filter(Tail, ["seller_id=" ++ to_string(SellerId) | Acc]);
search_filter([{category, CategoryId} | Tail], Acc) ->
    search_filter(Tail, ["category=" ++ to_string(CategoryId) | Acc]);
search_filter([{q, Query} | Tail], Acc) ->
    search_filter(Tail, ["q=" ++ url_encode(Query) | Acc]);
search_filter([{offset, Offset} | Tail], Acc) ->
    search_filter(Tail, ["offset=" ++ to_string(Offset) | Acc]);
search_filter([{limit, Limit} | Tail], Acc) ->
    search_filter(Tail, ["limit=" ++ to_string(Limit) | Acc]);
search_filter([], Acc) ->
    lists:reverse(Acc).


-spec current_user(mlapi_access_token()) -> response().
current_user(AccessToken) ->
    current_user(AccessToken, []).

-spec current_user(mlapi_access_token(), [option()]) -> response().
current_user(AccessToken, Options) ->
    request(?USERS "/me?access_token=" ++ url_encode(AccessToken), ?SET_RECORD(mlapi_user, Options)).



-spec user_listing_types(mlapi_user_id(), mlapi_access_token()) -> response().
user_listing_types(UserId, AccessToken) ->
    user_listing_types(UserId, AccessToken, []).

-spec user_listing_types(mlapi_user_id(), mlapi_access_token(), [option()]) -> response().
user_listing_types(UserId, AccessToken, Options) ->
    Path = io_lib:format(?USERS "/~s" ?AVAILABLE_LISTING_TYPES "?access_token=~s", [to_string(UserId), url_encode(AccessToken)]),
    request(Path, ?SET_RECORD(mlapi_item, Options)).


-spec user_items(mlapi_user_id(), mlapi_access_token()) -> response().
user_items(UserId, AccessToken) ->
    user_items(UserId, AccessToken, []).

-spec user_items(mlapi_user_id(), mlapi_access_token(), [option()]) -> response().
user_items(UserId, AccessToken, Options) ->
    Path = io_lib:format(?USERS "/~s" ?ITEMS "/search?access_token=~s", [to_string(UserId), url_encode(AccessToken)]),
    request(Path, ?SET_RECORD(mlapi_item, Options)).





-spec request(url_path()) -> response().
request(Path) ->
    request(Path, []).

-spec request(url_path(), [option()]) -> response().
request(Path, Options) ->
    case ibrowse:send_req(get_env(protocol, ?PROTOCOL) ++ "://" ++ get_env(host, ?HOST) ++ Path, [], get) of
        {ok, "200", Headers, Body} ->
            case lists:keyfind(?HEADER_CONTENT_TYPE, 1, Headers) of
                {_ContentType, ?MIME_TYPE_JSON ++ _CharSet} ->
                    case proplists:get_value(format, Options, get_env(default_format, ejson)) of
                        binary ->
                            Body;
                        Format ->
                            try
                                ejson_to_term(ejson:decode(Body), proplists:get_value(record, Options), Format)
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


-spec ejson_to_term(ejson(), RecordName :: atom(), format()) -> ejson() | orddict:orddict() | proplist() | tuple().
ejson_to_term(Doc, _RecordName, ejson) ->
    Doc;
ejson_to_term(Doc, RecordName, orddict) ->
    ejson_to_orddict(Doc, RecordName);
ejson_to_term(Doc, RecordName, proplist) ->
    ejson_to_proplist(Doc, RecordName);
ejson_to_term(Doc, RecordName, record) ->
    ejson_to_record(Doc, RecordName).


%% @doc Convert a parsed JSON document or a list of documents into one or more known record.
-spec ejson_to_record(tuple() | [tuple()], Record :: atom() | tuple()) -> tuple() | [tuple()].
ejson_to_record({Elements}, RecordOrName) when is_list(Elements) ->
    JsonHelperFun = #json_helper{
      child_to_term = fun ejson_to_record/2,
      append = fun (Name, Value, Record) -> set_value(Name, Value, Record) end,
      finish = fun (Record) -> Record end
     },
    {RecordName, Record} = if
                               is_tuple(RecordOrName) ->
                                   {element(1, RecordOrName), RecordOrName};
                               is_atom(RecordOrName) ->
                                   {RecordOrName, new_record(RecordOrName)}
                           end,
    ejson_list_to_term(RecordName, JsonHelperFun, Elements, Record);
ejson_to_record(Elements, RecordName) when is_list(Elements) ->
    lists:reverse(
      lists:foldl(fun (Element, Acc) ->
                          [ejson_to_record(Element, new_record(RecordName)) | Acc]
                  end, [], Elements)).


%% @doc Convert a parsed JSON document or a list of documents into one or more property lists.
-spec ejson_to_proplist(tuple() | [tuple()], RecordName :: atom()) -> proplist() | [proplist()].
ejson_to_proplist({Elements}, RecordName) when is_list(Elements) ->
    JsonHelperFun = #json_helper{
      child_to_term = fun ejson_to_proplist/2,
      append = fun (Name, Value, Acc) -> [{Name, Value} | Acc] end,
      finish = fun lists:reverse/1
     },
    ejson_list_to_term(RecordName, JsonHelperFun, Elements, []);
ejson_to_proplist(Elements, RecordName) when is_list(Elements) ->
    lists:reverse(
      lists:foldl(fun (Element, Acc) ->
                          [ejson_to_proplist(Element, RecordName) | Acc]
                  end, [], Elements)).


%% @doc Convert a parsed JSON document or a list of documents into one or more ordered dictionaries.
-spec ejson_to_orddict(tuple() | [tuple()], RecordName :: atom()) -> orddict:orddict() | [orddict:orddict()].
ejson_to_orddict({Elements}, RecordName) when is_list(Elements) ->
    JsonHelperFun = #json_helper{
      child_to_term = fun ejson_to_orddict/2,
      append = fun orddict:append/3,
      finish = fun (Dict) -> Dict end
     },
    ejson_list_to_term(RecordName, JsonHelperFun, Elements, orddict:new());
ejson_to_orddict(Elements, RecordName) when is_list(Elements) ->
    lists:reverse(
      lists:foldl(fun (Element, Acc) ->
                          [ejson_to_orddict(Element, RecordName) | Acc]
                  end, [], Elements)).

-spec ejson_list_to_term(RecordName :: atom(), #json_helper{}, [{binary(), any()}], tuple() | orddict:orddict() | proplist()) ->
                                tuple() | orddict:orddict() | proplist().
ejson_list_to_term(RecordName, JsonHelperFun, [{Name, Value} | Tail], Acc) ->
    FieldName = binary_to_existing_atom(Name, utf8),
    %% Convert the value to a record if possible
    NewValue =
        case ejson_field_to_record_name(RecordName, FieldName) of
            undefined ->
                case is_ejson_datetime_field(RecordName, FieldName) of
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
    ejson_list_to_term(RecordName, JsonHelperFun, Tail, (JsonHelperFun#json_helper.append)(FieldName, NewValue, Acc));
ejson_list_to_term(_RecordName, JsonHelperFun, [], Acc) ->
    (JsonHelperFun#json_helper.finish)(Acc).


%% @doc Return the record name for those JSON fields that can be converted to a known child record.
-spec ejson_field_to_record_name(ParentRecordName :: atom(), FieldName :: atom()) -> ChildRecordName :: atom() | undefined.
ejson_field_to_record_name(mlapi_buyer_reputation, transactions) ->
    mlapi_buyer_transactions;
ejson_field_to_record_name(mlapi_buyer_transactions, canceled) ->
    mlapi_buyer_transaction_count;
ejson_field_to_record_name(mlapi_buyer_transactions, unrated) ->
    mlapi_buyer_transaction_count;
ejson_field_to_record_name(mlapi_buyer_transactions, not_yet_rated) ->
    mlapi_buyer_transaction_count;
ejson_field_to_record_name(mlapi_category_ext, children_categories) ->
    mlapi_category;
ejson_field_to_record_name(mlapi_category_ext, settings) ->
    mlapi_settings;
ejson_field_to_record_name(mlapi_credit_level, exception_by_category) ->
    mlapi_credit_exception_by_category;
ejson_field_to_record_name(mlapi_country_ext, states) ->
    mlapi_state;
ejson_field_to_record_name(mlapi_domain, attributes) ->
    mlapi_domain_attribute;
ejson_field_to_record_name(mlapi_exceptions_by_card_issuer, card_issuer) ->
    mlapi_card_issuer;
ejson_field_to_record_name(mlapi_exceptions_by_card_issuer, payer_costs) ->
    mlapi_payer_costs;
ejson_field_to_record_name(mlapi_filter, values) ->
    mlapi_filter_value;
ejson_field_to_record_name(mlapi_geo_information, location) ->
    mlapi_location;
ejson_field_to_record_name(mlapi_item, attributes) ->
    mlapi_attribute;
ejson_field_to_record_name(mlapi_item, city) ->
    mlapi_city;
ejson_field_to_record_name(mlapi_item, country) ->
    mlapi_country;
ejson_field_to_record_name(mlapi_item, descriptions) ->
    mlapi_description;
ejson_field_to_record_name(mlapi_item, geolocation) ->
    mlapi_location;
ejson_field_to_record_name(mlapi_payment_method_ext, exceptions_by_card_issuer) ->
    mlapi_exceptions_by_card_issuer;
ejson_field_to_record_name(mlapi_item, pictures) ->
    mlapi_picture;
ejson_field_to_record_name(mlapi_question, answer) ->
    mlapi_answer;
ejson_field_to_record_name(mlapi_question_result, questions) ->
    mlapi_question;
ejson_field_to_record_name(mlapi_item, seller_address) ->
    mlapi_seller_address;
ejson_field_to_record_name(mlapi_item, shipping) ->
    mlapi_shipping;
ejson_field_to_record_name(mlapi_item, state) ->
    mlapi_state;
ejson_field_to_record_name(mlapi_seller_reputation, transactions) ->
    mlapi_seller_transactions;
ejson_field_to_record_name(mlapi_search_item, address) ->
    mlapi_search_address;
ejson_field_to_record_name(mlapi_search_item, attributes) ->
    mlapi_attribute;
ejson_field_to_record_name(mlapi_search_item, seller) ->
    mlapi_seller;
ejson_field_to_record_name(mlapi_search_item, installments) ->
    mlapi_installment;
ejson_field_to_record_name(mlapi_site_ext, categories) ->
    mlapi_category;
ejson_field_to_record_name(mlapi_site_ext, currencies) ->
    mlapi_currency;
ejson_field_to_record_name(mlapi_state_ext, cities) ->
    mlapi_city;
ejson_field_to_record_name(mlapi_search_result, filters) ->
    mlapi_filter;
ejson_field_to_record_name(mlapi_search_result, available_filters) ->
    mlapi_filter;
ejson_field_to_record_name(mlapi_search_result, paging) ->
    mlapi_paging;
ejson_field_to_record_name(mlapi_search_result, results) ->
    mlapi_search_item;
ejson_field_to_record_name(mlapi_search_result, seller) ->
    mlapi_seller;
ejson_field_to_record_name(mlapi_search_result, sort) ->
    mlapi_sort;
ejson_field_to_record_name(mlapi_search_result, available_sorts) ->
    mlapi_sort;
ejson_field_to_record_name(mlapi_user, identification) ->
    mlapi_identification;
ejson_field_to_record_name(mlapi_user, buyer_reputation) ->
    mlapi_buyer_reputation;
ejson_field_to_record_name(mlapi_user, phone) ->
    mlapi_phone;
ejson_field_to_record_name(mlapi_user, seller_reputation) ->
    mlapi_seller_reputation;
ejson_field_to_record_name(mlapi_user, status) ->
    mlapi_user_status;
ejson_field_to_record_name(mlapi_user, credit) ->
    mlapi_user_credit;
ejson_field_to_record_name(mlapi_user_status, list) ->
    mlapi_user_action_status;
ejson_field_to_record_name(mlapi_user_status, buy) ->
    mlapi_user_action_status;
ejson_field_to_record_name(mlapi_user_status, sell) ->
    mlapi_user_action_status;
ejson_field_to_record_name(_RecordName, geo_information) ->
    mlapi_geo_information;
ejson_field_to_record_name(_RecordName, _FieldName) ->
    undefined.


%% @doc Check whether a field of a record should be converted to a datetime.
-spec is_ejson_datetime_field(RecordName :: atom(), FieldName :: atom()) -> boolean().
is_ejson_datetime_field(mlapi_shipping_costs, time) ->
    true;
is_ejson_datetime_field(mlapi_item, start_time) ->
    true;
is_ejson_datetime_field(mlapi_item, stop_time) ->
    true;
is_ejson_datetime_field(mlapi_search_item, stop_time) ->
    true;
is_ejson_datetime_field(mlapi_user, registration_date) ->
    true;
is_ejson_datetime_field(_RecordName, _FieldName) ->
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

-spec url_encode(string() | binary() | integer() | float() | atom()) -> string().
url_encode(String) ->
    ibrowse_lib:url_encode(to_string(String)).


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
