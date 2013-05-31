%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011-2012 Juan Jose Comellas
%%% @doc Dynamic cache of MercadoLibre API responses stored in Mnesia that
%%%      mirrors the functions exported in the mlapi module.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(mlapi_cache).

-export([init/0, init/1, init_metatable/1, init_table/2,
         create_tables/1, create_metatable/1, create_table/2,
         upgrade_metatable/0, upgrade_table/1, tables/0,
         table_info/1, table_version/1, table_time_to_live/1,
         last_update_to_datetime/1, current_time_in_gregorian_seconds/0
        ]).
%% Public APIs
-export([applications/0, applications/1, application/1, application/2,
         catalog_products/2, catalog_products/3, catalog_product/2, catalog_product/3,
         sites/0, sites/1, site/1, site/2,
         countries/0, countries/1, country/1, country/2,
         state/1, state/2, city/1, city/2,
         currencies/0, currencies/1, currency/1, currency/2,
         currency_conversion/1, currency_conversion/2,
         listing_exposures/1, listing_exposures/2, listing_exposure/2, listing_exposure/3,
         listing_prices/2, listing_prices/3,
         listing_types/1, listing_types/2,
         payment_types/0, payment_types/1, payment_type/1, payment_type/2,
         payment_methods/1, payment_methods/2, payment_method/2, payment_method/3,
         card_issuers/1, card_issuers/2, card_issuer/2, card_issuer/3,
         credit_level/1, credit_level/2,
         category/1, category/2,
         domains/0, domains/1, domain/1, domain/2,
         user/1, user/2, user_by_nickname/1, user_by_nickname/2,
         item/1, item/2,
         picture/1, picture/2,
         question/1, question/2, questions/1, questions/2,
         trends/2, trends/3,
         local_geolocation/0, local_geolocation/1, geolocation/1, geolocation/2,
         search/2, search/3
        ]).
%% Private APIs
-export([my_archived_sales/2, my_archived_sales/3,
         my_active_sales/2, my_active_sales/3,
         my_sale/2, my_sale/3,
         my_orders/1, my_orders/2,
         my_archived_orders/1, my_archived_orders/2,
         my_order/2, my_order/3,
         my_user/1, my_user/2,
         user_listing_types/2, user_listing_types/3,
         user_items/2, user_items/3
        ]).
%% post_question/2, delete_question/2, post_answer/2, hide_questions/2,

-include("include/mlapi.hrl").
-include("include/mlapi_cache.hrl").

-define(META_VERSION, 1).
-define(MIN_IN_SECS,        60).
-define(HOUR_IN_SECS,     3600).
-define(DAY_IN_SECS,     86400).
-define(WEEK_IN_SECS,   604800).
-define(MONTH_IN_SECS, 2592000).
-define(YEAR_IN_SECS, 31536000).
%% Cache entries are kept for 1 hour by default (this can be overridden per table, see mlapi_metatable).
-define(DEFAULT_CACHE_TIME_TO_LIVE, ?HOUR_IN_SECS).

-record(table_info, {
          table                                  :: table(),
          version                                :: table_version(),
          time_to_live                           :: time_to_live()
         }).


-spec init() -> ok | {aborted, Reason :: any()}.
init() ->
    init(mlapi:get_env(cache_nodes, [node()])).


-spec init([node()]) -> ok | no_return(). %% exit({aborted, Reason :: any()}).
init(Nodes) ->
    init_metatable(Nodes),
    lists:foreach(fun (#table_info{table = Table, version = Version, time_to_live = TimeToLive}) ->
                          init_table(Table, Version, TimeToLive, Nodes)
                  end, tables()).


-spec init_metatable([node()]) -> ok | {aborted, Reason :: any()}.
init_metatable(Nodes) ->
    Fields = record_info(fields, mlapi_metatable),
    %% Make sure that the schema of the Mnesia table is up-to-date.
    try ((length(Fields) + 1 =:= mnesia:table_info(mlapi_metatable, arity)) andalso
         (Fields -- mnesia:table_info(mlapi_metatable, attributes)) =:= []) of
        true ->
            ok;
        false ->
            upgrade_metatable()
    catch
        _ : _ ->
            create_metatable(Nodes)
    end.

-spec init_table(table(), [node()]) -> ok | {aborted, Reason :: any()}.
init_table(Table, Nodes) ->
    {Version, TimeToLive} = table_info(Table),
    init_table(Table, Version, TimeToLive, Nodes).

-spec init_table(table(), table_version(), time_to_live(), [node()]) -> ok | {aborted, Reason :: any()}.
init_table(Table, Version, TimeToLive, Nodes) ->
    Fields = record_info(fields, mlapi_cache),
    OldVersion = case mnesia:dirty_read(mlapi_metatable, Table) of
                     [#mlapi_metatable{version = Number}] ->
                         Number;
                     [] ->
                         Version
                 end,
    %% Make sure that the schema of the Mnesia table is up-to-date.
    try ((OldVersion =:= Version) andalso
         (length(Fields) + 1 =:= mnesia:table_info(Table, arity)) andalso
         (Fields -- mnesia:table_info(Table, attributes)) =:= []) of
        true ->
            ok;
        false ->
            upgrade_table(Table, OldVersion, Version, Fields)
    catch
        _ : _ ->
            create_table(Table, Version, TimeToLive, Nodes)
    end.


-spec create_tables([node()]) -> ok | {aborted, Reason :: any()}.
create_tables(Nodes) ->
    create_metatable(Nodes),
    lists:foreach(fun (#table_info{table = Table, version = Version, time_to_live = TimeToLive}) ->
                          create_table(Table, Version, TimeToLive, Nodes)
                  end, tables()).


-spec create_metatable([node()]) -> ok | {aborted, Reason :: any()}.
create_metatable(Nodes) ->
    case mnesia:create_table(mlapi_metatable, [{access_mode, read_write},
                                               {record_name, mlapi_metatable},
                                               {attributes, record_info(fields, mlapi_metatable)},
                                               {disc_copies, Nodes},
                                               {type, set},
                                               {local_content, true}]) of
        {atomic, ok} ->
            ok;
        Error ->
            Error
    end.

-spec create_table(table(), [node()]) -> ok | {aborted, Reason :: any()}.
create_table(Table, Nodes) ->
    {Version, TimeToLive} = table_info(Table),
    create_table(Table, Version, TimeToLive, Nodes).

-spec create_table(table(), table_version(), time_to_live(), [node()]) -> ok | {aborted, Reason :: any()}.
create_table(Table, Version, TimeToLive, Nodes) ->
    case mnesia:create_table(Table, [{access_mode, read_write},
                                     {record_name, mlapi_cache},
                                     {attributes, record_info(fields, mlapi_cache)},
                                     {disc_copies, Nodes},
                                     {type, set},
                                     {local_content, true}]) of
        {atomic, ok} ->
            mnesia:dirty_write(mlapi_metatable,
                               #mlapi_metatable{
                                 table = Table,
                                 version = Version,
                                 time_to_live = TimeToLive,
                                 last_update = current_time_in_gregorian_seconds(),
                                 reason = create_table
                                });
        Error ->
            Error
    end.


-spec upgrade_metatable() -> {atomic, ok} | {aborted, Reason :: any()}.
upgrade_metatable() ->
    upgrade_table(mlapi_metatable, record_info(fields, mlapi_metatable)).


-spec upgrade_table(table()) -> {atomic, ok} | {aborted, Reason :: any()}.
upgrade_table(Table) ->
    upgrade_table(Table, record_info(fields, mlapi_cache)).


-spec upgrade_table(table(), [mlapi_field()]) -> {atomic, ok} | {aborted, Reason :: any()}.
upgrade_table(Table, Fields) ->
    %% Replace 'ignore' with a function that performs the schema upgrade once the schema changes.
    mnesia:transform_table(Table, ignore, Fields, Table).


-spec upgrade_table(table(), OldVersion :: non_neg_integer(), NewVersion :: non_neg_integer(), [mlapi_field()]) ->
                           {atomic, ok} | {aborted, Reason :: any()}.
upgrade_table(Table, _OldVersion, _NewVersion, Fields) ->
    %% Replace 'ignore' with a function that performs the schema upgrade once the schema changes.
    mnesia:transform_table(Table, ignore, Fields, Table).


-spec tables() -> [#table_info{}].
tables() ->
    [
     #table_info{table = mlapi_application,         version = 1, time_to_live = 3 * ?HOUR_IN_SECS},
     #table_info{table = mlapi_card_issuer,         version = 1, time_to_live = ?DAY_IN_SECS},
     #table_info{table = mlapi_catalog_product,     version = 1, time_to_live = ?WEEK_IN_SECS},
     #table_info{table = mlapi_category,            version = 1, time_to_live = ?WEEK_IN_SECS},
     #table_info{table = mlapi_city,                version = 1, time_to_live = ?WEEK_IN_SECS},
     #table_info{table = mlapi_country,             version = 1, time_to_live = ?MONTH_IN_SECS},
     #table_info{table = mlapi_credit_level,        version = 1, time_to_live = ?DAY_IN_SECS},
     #table_info{table = mlapi_currency,            version = 1, time_to_live = ?MONTH_IN_SECS},
     #table_info{table = mlapi_currency_conversion, version = 1, time_to_live = ?HOUR_IN_SECS},
     #table_info{table = mlapi_domain,              version = 1, time_to_live = ?WEEK_IN_SECS},
     #table_info{table = mlapi_geolocation,         version = 1, time_to_live = ?WEEK_IN_SECS},
     #table_info{table = mlapi_item,                version = 1, time_to_live = 30 * ?MIN_IN_SECS},
     #table_info{table = mlapi_listing_exposure,    version = 1, time_to_live = ?WEEK_IN_SECS},
     #table_info{table = mlapi_listing_price,       version = 1, time_to_live = ?DAY_IN_SECS},
     #table_info{table = mlapi_listing_type,        version = 1, time_to_live = ?WEEK_IN_SECS},
     #table_info{table = mlapi_order,               version = 1, time_to_live = 10 * ?MIN_IN_SECS},
     #table_info{table = mlapi_order_search,        version = 1, time_to_live = 10 * ?MIN_IN_SECS},
     #table_info{table = mlapi_payment_type,        version = 1, time_to_live = ?DAY_IN_SECS},
     #table_info{table = mlapi_payment_method,      version = 1, time_to_live = ?DAY_IN_SECS},
     #table_info{table = mlapi_picture,             version = 1, time_to_live = ?WEEK_IN_SECS},
     #table_info{table = mlapi_question,            version = 1, time_to_live = 10 * ?MIN_IN_SECS},
     #table_info{table = mlapi_sale,                version = 1, time_to_live = 10 * ?MIN_IN_SECS},
     #table_info{table = mlapi_search_result,       version = 1, time_to_live = 3 * ?HOUR_IN_SECS},
     #table_info{table = mlapi_site,                version = 1, time_to_live = ?MONTH_IN_SECS},
     #table_info{table = mlapi_state,               version = 1, time_to_live = ?WEEK_IN_SECS},
     #table_info{table = mlapi_trend,               version = 1, time_to_live = ?HOUR_IN_SECS},
     #table_info{table = mlapi_user,                version = 1, time_to_live = ?DAY_IN_SECS}
    ].


-spec table_info(table()) -> {table_version(), time_to_live()} | undefined.
table_info(Table) ->
    case lists:keyfind(Table, #table_info.table, tables()) of
        #table_info{version = Version, time_to_live = TimeToLive} ->
            {Version, TimeToLive};
        false ->
            undefined
    end.

-spec table_version(table()) -> table_version().
table_version(Table) ->
    case lists:keyfind(Table, #table_info.table, tables()) of
        #table_info{version = Version} ->
            Version;
        false ->
            undefined
    end.


-spec table_time_to_live(table()) -> time_to_live().
table_time_to_live(Table) ->
    case lists:keyfind(Table, #table_info.table, tables()) of
        #table_info{time_to_live = TimeToLive} ->
            TimeToLive;
        false ->
            mlapi:get_env(cache_time_to_live)
    end.

-spec last_update_to_datetime(last_update()) -> calendar:datetime().
last_update_to_datetime(LastUpdate) ->
    calendar:gregorian_seconds_to_datetime(LastUpdate).


-spec applications() -> mlapi:response().
applications() ->
    applications([]).

-spec applications([mlapi:option()]) -> mlapi:response().
applications(Options) ->
    get_data(mlapi_application, mlapi_application, applications, Options,
             fun (NewOptions) -> mlapi:applications(NewOptions) end).

-spec application(mlapi_application_id()) -> mlapi:response().
application(ApplicationId) ->
    application(ApplicationId, []).

-spec application(mlapi_application_id(), [mlapi:option()]) -> mlapi:response().
application(ApplicationId, Options) ->
    get_data(mlapi_application, mlapi_application, mlapi:to_binary(ApplicationId), Options,
             fun (NewOptions) -> mlapi:application(ApplicationId, NewOptions) end).


-spec catalog_products(mlapi_site_id(), [mlapi_catalog_product_arg()]) -> mlapi:response().
catalog_products(SiteId, Filter) ->
    catalog_products(SiteId, Filter, []).

-spec catalog_products(mlapi_site_id(), [mlapi_catalog_product_arg()], [mlapi:option()]) -> mlapi:response().
catalog_products(SiteId, Filter, Options) ->
    get_data(mlapi_catalog_product, mlapi_catalog_product_search, {mlapi:to_binary(SiteId), normalize_args(Filter)}, Options,
             fun (NewOptions) -> mlapi:catalog_products(SiteId, Filter, NewOptions) end).

-spec catalog_product(mlapi_site_id(), mlapi_catalog_product_id()) -> mlapi:response().
catalog_product(SiteId, CatalogProductId) ->
    catalog_product(SiteId, CatalogProductId, []).

-spec catalog_product(mlapi_site_id(), mlapi_catalog_product_id(), [mlapi:option()]) -> mlapi:response().
catalog_product(SiteId, CatalogProductId, Options) ->
    get_data(mlapi_catalog_product, mlapi_catalog_product, {mlapi:to_binary(SiteId), mlapi:to_binary(CatalogProductId)}, Options,
             fun (NewOptions) -> mlapi:catalog_product(SiteId, CatalogProductId, NewOptions) end).


-spec sites() -> mlapi:response().
sites() ->
    sites([]).

-spec sites([mlapi:option()]) -> mlapi:response().
sites(Options) ->
    get_data(mlapi_site, mlapi_site, sites, Options, fun (NewOptions) -> mlapi:sites(NewOptions) end).

-spec site(mlapi_site_id()) -> mlapi:response().
site(SiteId) ->
    site(SiteId, []).

-spec site(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
site(SiteId, Options) ->
    get_data(mlapi_site, mlapi_site_ext, mlapi:to_binary(SiteId), Options, fun (NewOptions) -> mlapi:site(SiteId, NewOptions) end).


-spec countries() -> mlapi:response().
countries() ->
    countries([]).

-spec countries([mlapi:option()]) -> mlapi:response().
countries(Options) ->
    get_data(mlapi_country, mlapi_country, countries, Options, fun (NewOptions) -> mlapi:countries(NewOptions) end).

-spec country(mlapi_country_id()) -> mlapi:response().
country(CountryId) ->
    country(CountryId, []).

-spec country(mlapi_country_id(), [mlapi:option()]) -> mlapi:response().
country(CountryId, Options) ->
    get_data(mlapi_country, mlapi_country_ext, mlapi:to_binary(CountryId), Options,
             fun (NewOptions) -> mlapi:country(CountryId, NewOptions) end).


-spec state(mlapi_state_id()) -> mlapi:response().
state(StateId) ->
    state(StateId, []).

-spec state(mlapi_state_id(), [mlapi:option()]) -> mlapi:response().
state(StateId, Options) ->
    get_data(mlapi_state, mlapi_state_ext, mlapi:to_binary(StateId), Options,
             fun (NewOptions) -> mlapi:state(StateId, NewOptions) end).


-spec city(mlapi_city_id()) -> mlapi:response().
city(CityId) ->
    city(CityId, []).

-spec city(mlapi_city_id(), [mlapi:option()]) -> mlapi:response().
city(CityId, Options) ->
    get_data(mlapi_city, mlapi_city_ext, mlapi:to_binary(CityId), Options,
             fun (NewOptions) -> mlapi:city(CityId, NewOptions) end).


-spec currencies() -> mlapi:response().
currencies() ->
    currencies([]).

-spec currencies([mlapi:option()]) -> mlapi:response().
currencies(Options) ->
    get_data(mlapi_currency, mlapi_currency, currencies, Options, fun (NewOptions) -> mlapi:currencies(NewOptions) end).


-spec currency(mlapi_currency_id()) -> mlapi:response().
currency(CurrencyId) ->
    currency(CurrencyId, []).

-spec currency(mlapi_currency_id(), [mlapi:option()]) -> mlapi:response().
currency(CurrencyId, Options) ->
    get_data(mlapi_currency, mlapi_currency, mlapi:to_binary(CurrencyId), Options,
             fun (NewOptions) -> mlapi:currency(CurrencyId, NewOptions) end).


-spec currency_conversion([mlapi_currency_conversion_arg()]) -> mlapi:response().
currency_conversion(Filter) ->
    currency_conversion(Filter, []).

-spec currency_conversion([mlapi_currency_conversion_arg()], [mlapi:option()]) -> mlapi:response().
currency_conversion(Filter, Options) ->
    NewFilter = case lists:keyfind(date, 1, Filter) of
                    {date, {Date, {Hour, Min, _Sec}}} ->
                        %% We assume that currency conversions are valid for 15 minutes.
                        Rem = Min rem 15,
                        lists:keystore(date, 1, Filter, {date, {Date, {Hour, Min - Rem, 0}}});
                    false ->
                        Filter
                end,
    get_data(mlapi_currency_conversion, mlapi_currency_conversion, normalize_args(NewFilter), Options,
             fun (NewOptions) -> mlapi:currency_conversion(Filter, NewOptions) end).


-spec listing_exposures(mlapi_site_id()) -> mlapi:response().
listing_exposures(SiteId) ->
    listing_exposures(SiteId, []).

-spec listing_exposures(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
listing_exposures(SiteId, Options) ->
    get_data(mlapi_listing_exposure, mlapi_listing_exposure, mlapi:to_binary(SiteId), Options,
             fun (NewOptions) -> mlapi:listing_exposures(SiteId, NewOptions) end).


-spec listing_exposure(mlapi_site_id(), mlapi_listing_exposure_id()) -> mlapi:response().
listing_exposure(SiteId, ListingExposureId) ->
    listing_exposure(SiteId, ListingExposureId, []).

-spec listing_exposure(mlapi_site_id(), mlapi_listing_exposure_id(), [mlapi:option()]) -> mlapi:response().
listing_exposure(SiteId, ListingExposureId, Options) ->
    get_data(mlapi_listing_exposure, mlapi_listing_exposure, {mlapi:to_binary(SiteId), mlapi:to_binary(ListingExposureId)}, Options,
             fun (NewOptions) -> mlapi:listing_exposure(SiteId, ListingExposureId, NewOptions) end).


-spec listing_prices(mlapi_site_id(), [mlapi_listing_price_arg()]) -> mlapi:response().
listing_prices(SiteId, Filter) ->
    listing_prices(SiteId, Filter, []).

-spec listing_prices(mlapi_site_id(), [mlapi_listing_price_arg()], [mlapi:option()]) -> mlapi:response().
listing_prices(SiteId, Filter, Options) ->
    get_data(mlapi_listing_price, mlapi_listing_price, normalize_args(Filter), Options,
             fun (NewOptions) -> mlapi:listing_prices(SiteId, Filter, NewOptions) end).


-spec listing_types(mlapi_site_id()) -> mlapi:response().
listing_types(SiteId) ->
    listing_types(SiteId, []).

-spec listing_types(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
listing_types(SiteId, Options) ->
    get_data(mlapi_listing_type, mlapi_listing_type, mlapi:to_binary(SiteId), Options,
             fun (NewOptions) -> mlapi:listing_types(SiteId, NewOptions) end).


-spec payment_types() -> mlapi:response().
payment_types() ->
    payment_types([]).

-spec payment_types([mlapi:option()]) -> mlapi:response().
payment_types(Options) ->
    get_data(mlapi_payment_type, mlapi_payment_type, payment_types, Options,
             fun (NewOptions) -> mlapi:payment_types(NewOptions) end).

-spec payment_type(mlapi_payment_type_id()) -> mlapi:response().
payment_type(PaymentTypeId) ->
    payment_type(PaymentTypeId, []).

-spec payment_type(mlapi_payment_type_id(), [mlapi:option()]) -> mlapi:response().
payment_type(PaymentTypeId, Options) ->
    get_data(mlapi_payment_type, mlapi_payment_type, mlapi:to_binary(PaymentTypeId), Options,
             fun (NewOptions) -> mlapi:payment_type(PaymentTypeId, NewOptions) end).


-spec payment_methods(mlapi_site_id()) -> mlapi:response().
payment_methods(SiteId) ->
    payment_methods(SiteId, []).

-spec payment_methods(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
payment_methods(SiteId, Options) ->
    get_data(mlapi_payment_method, mlapi_payment_method, mlapi:to_binary(SiteId), Options,
             fun (NewOptions) -> mlapi:payment_methods(SiteId, NewOptions) end).

-spec payment_method(mlapi_site_id(), mlapi_payment_method_id()) -> mlapi:response().
payment_method(SiteId, PaymentMethodId) ->
    payment_method(SiteId, PaymentMethodId, []).

-spec payment_method(mlapi_site_id(), mlapi_payment_method_id(), [mlapi:option()]) -> mlapi:response().
payment_method(SiteId, PaymentMethodId, Options) ->
    get_data(mlapi_payment_method, mlapi_payment_method_ext, {mlapi:to_binary(SiteId), mlapi:to_binary(PaymentMethodId)}, Options,
             fun (NewOptions) -> mlapi:payment_method(SiteId, PaymentMethodId, NewOptions) end).


-spec card_issuers(mlapi_site_id()) -> mlapi:response().
card_issuers(SiteId) ->
    card_issuers(SiteId, []).

-spec card_issuers(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
card_issuers(SiteId, Options) ->
    get_data(mlapi_card_issuer, mlapi_card_issuer, mlapi:to_binary(SiteId), Options,
             fun (NewOptions) -> mlapi:card_issuers(SiteId, NewOptions) end).

-spec card_issuer(mlapi_site_id(), mlapi_card_issuer_id()) -> mlapi:response().
card_issuer(SiteId, CardIssuerId) ->
    card_issuer(SiteId, CardIssuerId, []).

-spec card_issuer(mlapi_site_id(), mlapi_card_issuer_id(), [mlapi:option()]) -> mlapi:response().
card_issuer(SiteId, CardIssuerId, Options) ->
    get_data(mlapi_card_issuer, mlapi_card_issuer_ext, {mlapi:to_binary(SiteId), mlapi:to_binary(CardIssuerId)}, Options,
             fun (NewOptions) -> mlapi:card_issuer(SiteId, CardIssuerId, NewOptions) end).


-spec credit_level(mlapi_credit_level_id()) -> mlapi:response().
credit_level(CreditLevelId) ->
    credit_level(CreditLevelId, []).

-spec credit_level(mlapi_credit_level_id(), [mlapi:option()]) -> mlapi:response().
credit_level(CreditLevelId, Options) ->
    get_data(mlapi_credit_level, mlapi_credit_level, mlapi:to_binary(CreditLevelId), Options,
             fun (NewOptions) -> mlapi:credit_level(CreditLevelId, NewOptions) end).


-spec category(mlapi_category_id()) -> mlapi:response().
category(CategoryId) ->
    category(CategoryId, []).

-spec category(mlapi_category_id(), [mlapi:option()]) -> mlapi:response().
category(CategoryId, Options) ->
    get_data(mlapi_category, mlapi_category, mlapi:to_binary(CategoryId), Options,
             fun (NewOptions) -> mlapi:category(CategoryId, NewOptions) end).


-spec domains() -> mlapi:response().
domains() ->
    domains([]).

-spec domains([mlapi:option()]) -> mlapi:response().
domains(Options) ->
    get_data(mlapi_domain, mlapi_domain, domains, Options, fun (NewOptions) -> mlapi:domains(NewOptions) end).


-spec domain(mlapi_domain_id()) -> mlapi:response().
domain(DomainId) ->
    domain(DomainId, []).

-spec domain(mlapi_domain_id(), [mlapi:option()]) -> mlapi:response().
domain(DomainId, Options) ->
    get_data(mlapi_domain, mlapi_domain, mlapi:to_binary(DomainId), Options,
             fun (NewOptions) -> mlapi:domain(DomainId, NewOptions) end).


-spec user(mlapi_user_id()) -> mlapi:response().
user(UserId) ->
    user(UserId, []).

-spec user(mlapi_user_id(), [mlapi:option()]) -> mlapi:response().
user(UserId, Options) ->
    get_data(mlapi_user, mlapi_user, mlapi:to_binary(UserId), Options, fun (NewOptions) -> mlapi:user(UserId, NewOptions) end).


-spec user_by_nickname(mlapi_user_nickname()) -> mlapi:response().
user_by_nickname(Nickname) ->
    user_by_nickname(Nickname, []).

-spec user_by_nickname(mlapi_user_nickname(), [mlapi:option()]) -> mlapi:response().
user_by_nickname(Nickname, Options) ->
    get_data(mlapi_user, mlapi_user, mlapi:to_binary(Nickname), Options,
             fun (NewOptions) -> mlapi:user_by_nickname(Nickname, NewOptions) end).


-spec item(mlapi_item_id()) -> mlapi:response().
item(ItemId) ->
    item(ItemId, []).

-spec item(mlapi_item_id(), [mlapi:option()]) -> mlapi:response().
item(ItemId, Options) ->
    get_data(mlapi_item, mlapi_item, mlapi:to_binary(ItemId), Options, fun (NewOptions) -> mlapi:item(ItemId, NewOptions) end).


-spec picture(mlapi_picture_id()) -> mlapi:response().
picture(PictureId) ->
    picture(PictureId, []).

-spec picture(mlapi_picture_id(), [mlapi:option()]) -> mlapi:response().
picture(PictureId, Options) ->
    get_data(mlapi_picture, mlapi_picture, mlapi:to_binary(PictureId), Options,
             fun (NewOptions) -> mlapi:picture(PictureId, NewOptions) end).


-spec question(mlapi_question_id()) -> mlapi:response().
question(QuestionId) ->
    question(QuestionId, []).

-spec question(mlapi_question_id(), [mlapi:option()]) -> mlapi:response().
question(QuestionId, Options) ->
    get_data(mlapi_question, mlapi_question, mlapi:to_binary(QuestionId), Options,
             fun (NewOptions) -> mlapi:question(QuestionId, NewOptions) end).


-spec questions([mlapi_question_arg()]) -> mlapi:response().
questions(Filter) ->
    questions(Filter, []).

-spec questions([mlapi_question_arg()], [mlapi:option()]) -> mlapi:response().
questions(Filter, Options) ->
    get_data(mlapi_question, mlapi_question_result, normalize_args(Filter), Options,
             fun (NewOptions) -> mlapi:questions(Filter, NewOptions) end).


-spec trends(mlapi_site_id(), [mlapi_trend_arg()]) -> mlapi:response().
trends(SiteId, Filter) ->
    trends(SiteId, Filter, []).

-spec trends(mlapi_site_id(), [mlapi_trend_arg()], [mlapi:option()]) -> mlapi:response().
trends(SiteId, Filter, Options) ->
    get_data(mlapi_trend, mlapi_trend, {SiteId, normalize_args(Filter)}, Options,
             fun (NewOptions) -> mlapi:trends(SiteId, Filter, NewOptions) end).


-spec local_geolocation() -> mlapi:response().
local_geolocation() ->
    local_geolocation([]).

-spec local_geolocation([mlapi:option()]) -> mlapi:response().
local_geolocation(Options) ->
    get_data(mlapi_geolocation, mlapi_geolocation, whereami, Options,
             fun (NewOptions) -> mlapi:local_geolocation(NewOptions) end).

-spec geolocation(mlapi_ip_address()) -> mlapi:response().
geolocation(IpAddr) ->
    geolocation(IpAddr, []).

-spec geolocation(mlapi_ip_address(), [mlapi:option()]) -> mlapi:response().
geolocation(IpAddr, Options) ->
    get_data(mlapi_geolocation, mlapi_geolocation, IpAddr, Options,
             fun (NewOptions) -> mlapi:geolocation(IpAddr, NewOptions) end).


-spec search(mlapi_site_id(), [mlapi_search_arg()]) -> mlapi:response().
search(SiteId, Filter) ->
    search(SiteId, Filter, []).

-spec search(mlapi_site_id(), [mlapi_search_arg()], [mlapi:option()]) -> mlapi:response().
search(SiteId, Filter, Options) ->
    get_data(mlapi_search_result, mlapi_search_result, {SiteId, normalize_args(Filter)}, Options,
             fun (NewOptions) -> mlapi:search(SiteId, Filter, NewOptions) end).


-spec my_archived_sales(mlapi_access_token(), [mlapi_sale_arg()]) -> mlapi:response().
my_archived_sales(AccessToken, Filter) ->
    my_archived_sales(AccessToken, Filter, []).

-spec my_archived_sales(mlapi_access_token(), [mlapi_sale_arg()], [mlapi:option()]) -> mlapi:response().
my_archived_sales(AccessToken, Filter, Options) ->
    NewFilter = lists:keystore(access_token, 1, Filter, {access_token, AccessToken}),
    get_data(mlapi_sale, mlapi_sale, {my_archived_sales, normalize_args(NewFilter)}, Options,
             fun (NewOptions) -> mlapi:my_archived_sales(AccessToken, Filter, NewOptions) end).


-spec my_active_sales(mlapi_access_token(), [mlapi_sale_arg()]) -> mlapi:response().
my_active_sales(AccessToken, Filter) ->
    my_active_sales(AccessToken, Filter, []).

-spec my_active_sales(mlapi_access_token(), [mlapi_sale_arg()], [mlapi:option()]) -> mlapi:response().
my_active_sales(AccessToken, Filter, Options) ->
    NewFilter = lists:keystore(access_token, 1, Filter, {access_token, AccessToken}),
    get_data(mlapi_sale, mlapi_sale, {my_active_sales, normalize_args(NewFilter)}, Options,
             fun (NewOptions) -> mlapi:my_active_sales(AccessToken, Filter, NewOptions) end).


-spec my_orders([mlapi_order_arg()]) -> mlapi:response().
my_orders(Args) ->
    my_orders(Args, []).

-spec my_orders([mlapi_order_arg()], [mlapi:option()]) -> mlapi:response().
my_orders(Args, Options) ->
    get_data(mlapi_order_search, mlapi_order_search, {my_orders, normalize_args(Args)}, Options,
             fun (NewOptions) -> mlapi:my_orders(Args, NewOptions) end).


-spec my_archived_orders([mlapi_order_arg()]) -> mlapi:response().
my_archived_orders(Args) ->
    my_archived_orders(Args, []).

-spec my_archived_orders([mlapi_order_arg()], [mlapi:option()]) -> mlapi:response().
my_archived_orders(Args, Options) ->
    get_data(mlapi_order_search, mlapi_order_search, {my_archived_orders, normalize_args(Args)}, Options,
             fun (NewOptions) -> mlapi:my_archived_orders(Args, NewOptions) end).


-spec my_order(mlapi_order_id(), mlapi_access_token()) -> mlapi:response().
my_order(OrderId, AccessToken) ->
    my_order(OrderId, AccessToken, []).

-spec my_order(mlapi_order_id(), mlapi_access_token(), [mlapi:option()]) -> mlapi:response().
my_order(OrderId, AccessToken, Options) ->
    get_data(mlapi_order, mlapi_order, {mlapi:to_binary(OrderId), mlapi:to_binary(AccessToken)}, Options,
             fun (NewOptions) -> mlapi:my_order(OrderId, AccessToken, NewOptions) end).


-spec my_sale(mlapi_sale_id(), mlapi_access_token()) -> mlapi:response().
my_sale(SaleId, AccessToken) ->
    my_sale(SaleId, AccessToken, []).

-spec my_sale(mlapi_sale_id(), mlapi_access_token(), [mlapi:option()]) -> mlapi:response().
my_sale(SaleId, AccessToken, Options) ->
    get_data(mlapi_sale, mlapi_sale, {mlapi:to_binary(SaleId), mlapi:to_binary(AccessToken)}, Options,
             fun (NewOptions) -> mlapi:my_sale(SaleId, AccessToken, NewOptions) end).


-spec my_user(mlapi_access_token()) -> mlapi:response().
my_user(AccessToken) ->
    my_user(AccessToken, []).

-spec my_user(mlapi_access_token(), [mlapi:option()]) -> mlapi:response().
my_user(AccessToken, Options) ->
    get_data(mlapi_user, mlapi_user, {my_user, mlapi:to_binary(AccessToken)}, Options,
             fun (NewOptions) -> mlapi:my_user(AccessToken, NewOptions) end).


-spec user_listing_types(mlapi_user_id(), mlapi_access_token()) -> mlapi:response().
user_listing_types(UserId, AccessToken) ->
    user_listing_types(UserId, AccessToken, []).

-spec user_listing_types(mlapi_user_id(), mlapi_access_token(), [mlapi:option()]) -> mlapi:response().
user_listing_types(UserId, AccessToken, Options) ->
    get_data(mlapi_listing_type, mlapi_listing_type, {mlapi:to_binary(UserId), mlapi:to_binary(AccessToken)}, Options,
             fun (NewOptions) -> mlapi:user_listing_types(UserId, AccessToken, NewOptions) end).


-spec user_items(mlapi_user_id(), mlapi_access_token()) -> mlapi:response().
user_items(UserId, AccessToken) ->
    user_items(UserId, AccessToken, []).

-spec user_items(mlapi_user_id(), mlapi_access_token(), [mlapi:option()]) -> mlapi:response().
user_items(UserId, AccessToken, Options) ->
    get_data(mlapi_item, mlapi_item, {mlapi:to_binary(UserId), mlapi:to_binary(AccessToken)}, Options,
             fun (NewOptions) -> mlapi:user_items(UserId, AccessToken, NewOptions) end).



-spec get_data(table(), mlapi_record_name(), table_key(), [mlapi:option()], RefreshFun :: fun()) -> mlapi:response().
get_data(Table, RecordName, Key, Options, RefreshFun) ->
    CurrentTime = current_time_in_gregorian_seconds(),
    %% As we cache responses as raw JSON documents (binary) we need to remove
    %% the format from the list of Options and apply it manually before returning
    %% the response.
    {OutputFormat, PartialOptions} = split_format_option(Options),
    NewOptions = [{output_format, raw} | PartialOptions],
    Json = case lists:keyfind(refresh, 1, NewOptions) of
               {refresh, true} ->
                   get_fresh_data(Table, Key, NewOptions, RefreshFun, CurrentTime);
               _ ->
                   {LastUpdate, CachedJson} = cache_entry(Table, Key),
                   %% We store the datetime as seconds in the Gregorian calendar (since Jan 1, 0001 at 00:00:00).
                   case is_cache_valid(Table, LastUpdate, CurrentTime) of
                       true ->
                           CachedJson;
                       false ->
                           get_fresh_data(Table, Key, NewOptions, RefreshFun, CurrentTime)
                   end
           end,
    case Json of
        Json when is_binary(Json) andalso OutputFormat =/= raw ->
            DateFormat = proplists:get_value(date_format, NewOptions, mlapi:get_env(date_format, datetime)),
            mlapi_codec:decode(Json, [{record, RecordName}, {output_format, OutputFormat},
                                      {date_format, DateFormat}]);
        _ ->
            %% Return either the raw JSON binary or the error returned by MLAPI
            Json
    end.


-spec get_fresh_data(table(), table_key(), [mlapi:option()], RefreshFun :: fun(),
                     CurrentTime :: non_neg_integer()) -> mlapi:response().
get_fresh_data(Table, Key, Options, RefreshFun, CurrentTime) ->
    case RefreshFun(Options) of
        {error, _Reason} = Error ->
            Error;
        Data ->
            %% mnesia:dirty_write(Table, #mlapi_cache{key = Key,
            %%                                        last_update = CurrentTime,
            %%                                        data = Data
            %%                                       })
            %% Write the entry in a separate process to speed up the response to the caller.
            %% There's no need to supervise this process; if it fails we'll just refresh
            %% the cache the next time.
            WriteFun = fun () ->
                               mnesia:write(Table, #mlapi_cache{key = Key,
                                                                last_update = CurrentTime,
                                                                data = Data
                                                               }, write)
                       end,
            proc_lib:spawn(fun () -> {atomic, _Result} = mnesia:transaction(WriteFun) end),
            Data
    end.


-spec cache_entry(table(), table_key()) -> {last_update() | 'undefined', Data :: any() | 'undefined'} | no_return().
cache_entry(Table, Key) ->
    case mnesia:dirty_read(Table, Key) of
        [#mlapi_cache{last_update = LastUpdate, data = Data}] ->
            {LastUpdate, Data};
        [] ->
            {undefined, undefined}
    end.


-spec cache_time_to_live(table()) -> time_to_live().
cache_time_to_live(Table) ->
    case mnesia:dirty_read(mlapi_metatable, Table) of
        [#mlapi_metatable{time_to_live = TimeToLive}] ->
            TimeToLive;
        [] ->
            mlapi:get_env(cache_time_to_live, ?DEFAULT_CACHE_TIME_TO_LIVE)
    end.


-spec is_cache_valid(table(), last_update() | 'undefined', timestamp()) -> boolean().
is_cache_valid(_Table, undefined, _CurrentTime) ->
    %% The entry was never updated.
    false;
is_cache_valid(Table, LastUpdate, CurrentTime) ->
    LastUpdate + cache_time_to_live(Table) > CurrentTime.


%% @doc Normalizes a question, trend or search filter by converting all the
%%      values in the key/value pairs to binaries and sorting the property list.
-spec normalize_args([mlapi_question_arg()] | [mlapi_trend_arg()] | [mlapi_search_arg()]) -> [{atom(), binary()}].
normalize_args(Filter) when is_list(Filter)->
    normalize_args(Filter, []).

normalize_args([{Key, Value} | Tail], Acc) ->
    normalize_args(Tail, [{Key, mlapi:to_binary(Value)} | Acc]);
normalize_args([], Acc) ->
    lists:sort(Acc).


-spec split_format_option([mlapi:option()]) -> {mlapi:output_format(), [mlapi:option()]}.
split_format_option(Options) ->
    case lists:keytake(output_format, 1, Options) of
        {value, {output_format, OutputFormat}, NewOptions} ->
            {OutputFormat, NewOptions};
        false ->
            {mlapi:get_env(output_format, proplist), Options}
    end.


-spec current_time_in_gregorian_seconds() -> non_neg_integer().
current_time_in_gregorian_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
