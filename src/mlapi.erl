%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011 Juan Jose Comellas
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

-export([start/0, stop/0, request/1]).
-export([get_sites/0, get_site/1,
         get_countries/0, get_country/1,
         get_state/1, get_city/1, get_neighborhood/1,
         get_currencies/0, get_currency/1,
         get_listing_exposures/1, get_listing_exposure/2,
         get_listing_types/1, get_listing_prices/1,
         get_payment_types/0, get_payment_type/1,
         get_payment_methods/1, get_payment_method/2,
         get_categories/1, get_subcategories/1, get_category/1,
         get_user/1,
         get_item/1,
         get_picture/1,
         search/2, search/4,
         search_category/2, search_category/4,
         search_seller_id/2, search_seller_id/4,
         search_nickname/2, search_nickname/4]).
-export([json_to_record/2, json_to_proplist/2,
         json_field_to_record_name/2,
         is_json_datetime_field/2, iso_datetime_to_tuple/1]).

-include("include/mlapi.hrl").
-compile({parse_transform, dynarec}).

-type url_path()          :: string().
-type error()             :: {error, Reason :: atom() | {atom(), any()}}.
-type response()          :: {ok, mlapi_json:ejson()} | error().
-type site_id()           :: binary() | string().
-type country_id()        :: binary() | string().
-type state_id()          :: binary() | string().
-type city_id()           :: binary() | string().
-type neighborhood_id()   :: binary() | string().
-type currency_id()       :: binary() | string().
-type payment_method_id() :: binary() | string().
-type category_id()       :: binary() | string().

-export_type([error/0, response/0]).
-export_type([site_id/0, country_id/0, state_id/0, city_id/0, neighborhood_id/0,
              currency_id/0, payment_method_id/0, category_id/0]).

-define(PROTOCOL, "https").
-define(HOST, "api.mercadolibre.com").
-define(HEADER_CONTENT_TYPE, "Content-Type").
-define(MIME_TYPE_JSON, "application/json").

-define(SITES,             "/sites").
-define(COUNTRIES,         "/countries").
-define(STATES,            "/states").
-define(CITIES,            "/cities").
-define(NEIGHBORHOODS,     "/neighborhoods").
-define(CURRENCIES,        "/currencies").
-define(LISTING_EXPOSURES, "/listing_exposures").
-define(LISTING_TYPES,     "/listing_types").
-define(LISTING_PRICES,    "/listing_prices").
-define(PAYMENT_TYPES,     "/payment_types").
-define(PAYMENT_METHODS,   "/payment_methods").
-define(CATEGORIES,        "/categories").
-define(USERS,             "/users").
-define(ITEMS,             "/items").
-define(PICTURES,          "/pictures").
-define(SEARCH,            "/search").


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


-spec get_listing_exposures(SiteId :: string() | binary()) -> response().
get_listing_exposures(SiteId) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?LISTING_EXPOSURES).

-spec get_listing_exposure(SiteId :: string() | binary(), ListingExposureId :: string() | binary()) -> response().
get_listing_exposure(SiteId, ListingExposureId) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?LISTING_EXPOSURES "/" ++ to_string(ListingExposureId)).

-spec get_listing_types(SiteId :: string() | binary()) -> response().
get_listing_types(SiteId) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?LISTING_TYPES).

-spec get_listing_prices(SiteId :: string() | binary()) -> response().
get_listing_prices(SiteId) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?LISTING_PRICES "?price=1").


-spec get_payment_types() -> response().
get_payment_types() ->
    request(?PAYMENT_TYPES).

-spec get_payment_type(PaymentTypeId :: string() | binary()) -> response().
get_payment_type(PaymentTypeId) ->
    request(?PAYMENT_TYPES "/" ++ to_string(PaymentTypeId)).

-spec get_payment_methods(SiteId :: string() | binary()) -> response().
get_payment_methods(SiteId) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?PAYMENT_METHODS).

-spec get_payment_method(SiteId :: string() | binary(), PaymentMethodId :: string() | binary()) -> response().
get_payment_method(SiteId, PaymentMethodId) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?PAYMENT_METHODS "/" ++ to_string(PaymentMethodId)).


-spec get_categories(SiteId :: string() | binary()) -> response().
get_categories(SiteId) ->
    case get_site(SiteId) of
        {ok, Site} ->
            {ok, json_xpath:get([<<"categories">>], Site)};
        Error ->
            Error
    end.

-spec get_subcategories(ParentCategoryId :: string() | binary()) -> response().
get_subcategories(ParentCategoryId) ->
    request(?CATEGORIES "/" ++ to_string(ParentCategoryId)).


-spec get_category(CategoryId :: string() | binary()) -> response().
get_category(CategoryId) ->
    request(?CATEGORIES "/" ++ to_string(CategoryId)).


-spec get_user(UserId :: string() | binary()) -> response().
get_user(UserId) ->
    request(?USERS "/" ++ to_string(UserId)).


-spec get_item(ItemId :: string()) -> response().
get_item(ItemId) ->
    request(?ITEMS "/" ++ to_string(ItemId)).


-spec get_picture(PictureId :: string()) -> response().
get_picture(PictureId) ->
    request(?PICTURES "/" ++ to_string(PictureId)).


-spec search(SiteId :: string(), Query :: string()) -> response().
search(SiteId, Query) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?SEARCH ++ "?q=" ++ ibrowse_lib:url_encode(to_string(Query))).

-spec search(SiteId :: string(), Query :: string(),
             Offset :: non_neg_integer(), Limit :: non_neg_integer()) -> response().
search(SiteId, Query, Offset, Limit) ->
    request(io_lib:format(?SITES "/~s" ?SEARCH "?q=~s&offset=~w&limit=~w",
                          [SiteId, ibrowse_lib:url_encode(to_string(Query)), Offset, Limit])).


-spec search_category(SiteId :: string(), CategoryId :: string()) -> response().
search_category(SiteId, CategoryId) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?SEARCH "?category=" ++ ibrowse_lib:url_encode(to_string(CategoryId))).

-spec search_category(SiteId :: string(), CategoryId :: string(),
                      Offset :: non_neg_integer(), Limit :: non_neg_integer()) -> response().
search_category(SiteId, CategoryId, Offset, Limit) ->
    request(io_lib:format(?SITES "/~s" ?SEARCH "?category=~s&offset=~w&limit=~w",
                          [SiteId, ibrowse_lib:url_encode(to_string(CategoryId)), Offset, Limit])).


-spec search_seller_id(SiteId :: string(), SellerId :: string()) -> response().
search_seller_id(SiteId, SellerId) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?SEARCH "?seller_id=" ++ ibrowse_lib:url_encode(to_string(SellerId))).

-spec search_seller_id(SiteId :: string(), SellerId :: string(),
                       Offset :: non_neg_integer(), Limit :: non_neg_integer()) -> response().
search_seller_id(SiteId, SellerId, Offset, Limit) ->
    request(io_lib:format(?SITES "/~s" ?SEARCH "?seller_id=~s&offset=~w&limit=~w",
                          [SiteId, ibrowse_lib:url_encode(to_string(SellerId)), Offset, Limit])).


-spec search_nickname(SiteId :: string(), Nickname :: string()) -> response().
search_nickname(SiteId, Nickname) ->
    request(?SITES "/" ++ to_string(SiteId) ++ ?SEARCH "?nickname=" ++ ibrowse_lib:url_encode(to_string(Nickname))).

-spec search_nickname(SiteId :: string(), Nickname :: string(),
                      Offset :: non_neg_integer(), Limit :: non_neg_integer()) -> response().
search_nickname(SiteId, Nickname, Offset, Limit) ->
    request(io_lib:format(?SITES "/~s" ?SEARCH "?nickname=~s&offset=~w&limit=~w",
                          [SiteId, ibrowse_lib:url_encode(to_string(Nickname)), Offset, Limit])).


-spec request(url_path()) -> response().
request(Path) ->
    case ibrowse:send_req(?PROTOCOL "://" ?HOST ++ Path, [], get) of
        {ok, "200", Headers, Body} ->
            case lists:keyfind(?HEADER_CONTENT_TYPE, 1, Headers) of
                {_ContentType, ?MIME_TYPE_JSON ++ _CharSet} ->
                    try
                        ejson:decode(Body)
                    catch
                        throw:Reason ->
                            {error, Reason}
                    end;
                InvalidContentType ->
                    {error, {invalid_content_type, InvalidContentType}}
            end;
        {ok, Code, _Headers, _Body} ->
            {error, response_reason(Code)};

        {error, _Reason} = Error ->
            Error
    end.


%% @doc Convert a list of JSON elements into a list of known records.
%-spec json_to_record_list([tuple()], RecordName :: atom()) -> [tuple()].
%json_to_record_list(Elements, RecordName) when is_list(Elements) ->
%    lists:reverse(
%      lists:foldl(fun (Element, Acc) ->
%                          [json_to_record(Element, new_record(RecordName)) | Acc]
%                  end, [], Elements)).


%% @doc Convert a JSON element into a known record.
-spec json_to_record(tuple(), RecordName :: atom()) -> tuple().
json_to_record({Elements}, RecordName) when is_list(Elements), is_atom(RecordName) ->
    json_to_record_1(Elements, new_record(RecordName));
json_to_record({Elements}, Record) when is_list(Elements) ->
    json_to_record_1(Elements, Record);
json_to_record(Elements, RecordName) when is_list(Elements) ->
    lists:reverse(
      lists:foldl(fun (Element, Acc) ->
                          [json_to_record(Element, new_record(RecordName)) | Acc]
                  end, [], Elements)).

json_to_record_1([{Name, Value} | Tail], Record) ->
    FieldName = binary_to_existing_atom(Name, utf8),
    %% Convert the value to a record if possible
    NewValue =
        case json_field_to_record_name(element(1, Record), FieldName) of
            undefined ->
                case is_json_datetime_field(element(1, Record), FieldName) of
                    true ->
                        iso_datetime_to_tuple(Value);
                    false ->
                        Value
                end;
            RecordName ->
                if
                    is_tuple(Value) orelse is_list(Value) ->
                        json_to_record(Value, RecordName);
                    true ->
                        Value
                end
        end,
    json_to_record_1(Tail, set_value(element(1, Record), FieldName, Record, NewValue));
json_to_record_1([], Record) ->
    Record.


%% @doc Convert a JSON element into a property list.
-spec json_to_proplist(tuple(), RecordName :: atom()) -> tuple().
json_to_proplist({Elements}, RecordName) when is_list(Elements), is_atom(RecordName) ->
    json_to_proplist_1(Elements, RecordName, []);
json_to_proplist(Elements, RecordName) when is_list(Elements) ->
    lists:reverse(
      lists:foldl(fun (Element, Acc) ->
                          [json_to_proplist(Element, RecordName) | Acc]
                  end, [], Elements)).

json_to_proplist_1([{Name, Value} | Tail], RecordName, Acc) ->
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
                        json_to_proplist(Value, ChildRecordName);
                    true ->
                        Value
                end
        end,
    json_to_proplist_1(Tail, RecordName, [{FieldName, NewValue} | Acc]);
json_to_proplist_1([], _RecordName, Acc) ->
    lists:reverse(Acc).


%% @doc Return the record name for those JSON fields that can be converted to a known child record.
-spec json_field_to_record_name(ParentRecordName :: atom(), FieldName :: atom()) -> ChildRecordName :: atom() | undefined.
json_field_to_record_name(mlapi_buyer_reputation, transactions) ->
    mlapi_transactions;
json_field_to_record_name(mlapi_category, children_categories) ->
    mlapi_category;
json_field_to_record_name(mlapi_category, settings) ->
    mlapi_settings;
json_field_to_record_name(mlapi_country, states) ->
    mlapi_state;
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
json_field_to_record_name(mlapi_site, categories) ->
    mlapi_category;
json_field_to_record_name(mlapi_site, currencies) ->
    mlapi_currency;
json_field_to_record_name(mlapi_state, cities) ->
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
is_json_datetime_field(_RecordName, _FieldName) ->
    false.


%% @doc Convert a datetime in the ISO format to a datetime tuple.
-spec iso_datetime_to_tuple(binary()) -> calendar:datetime() | binary().
iso_datetime_to_tuple(<<Year:4/binary, $-, Month:2/binary, $-, Day:2/binary, $T,
                        Hour:2/binary, $:, Min:2/binary, $:, Sec:2/binary, $., _Millisec:3/binary, $Z>>) ->
    {{bstr:to_integer(Year), bstr:to_integer(Month), bstr:to_integer(Day)},
     {bstr:to_integer(Hour), bstr:to_integer(Min), bstr:to_integer(Sec)}};
iso_datetime_to_tuple(<<>>) ->
    undefined;
iso_datetime_to_tuple(Value) ->
    Value.


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
