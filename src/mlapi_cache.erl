%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc Dynamic cache of MercadoLibre API responses stored in Mnesia.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(mlapi_cache).

-export([init/0, init/1, init_metatable/1, init_table/3,
         create_tables/1, create_metatable/1, create_table/2,
         upgrade_metatable/0, upgrade_table/1, tables/0, table_info/1, table_version/1, table_ttl/1]).
-export([sites/0, sites/1, site/1, site/2,
         countries/0, countries/1, country/1, country/2,
         state/1, state/2, city/1, city/2,
         currencies/0, currencies/1, currency/1, currency/2,
         currency_conversion/2, currency_conversion/3, currency_conversion/4,
         listing_exposures/1, listing_exposures/2, listing_exposure/2, listing_exposure/3,
         listing_types/1, listing_types/2, listing_prices/1, listing_prices/2,
         payment_types/0, payment_types/1, payment_type/1, payment_type/2,
         payment_methods/1, payment_methods/2, payment_method/2, payment_method/3,
         card_issuers/1, card_issuers/2, card_issuer/2, card_issuer/3,
         category/1, category/2,
         user/1, user/2,
         item/1, item/2,
         picture/1, picture/2,
         trends/1, trends/2, category_trends/2, category_trends/3, category_trends/4,
         local_geolocation/0, local_geolocation/1, geolocation/1, geolocation/2,
         search/2, search/3, search/4, search/5,
         search_category/2, search_category/3, search_category/4, search_category/5,
         search_seller_id/2, search_seller_id/3, search_seller_id/4, search_seller_id/5,
         search_seller_nick/2, search_seller_nick/3, search_seller_nick/4, search_seller_nick/5
        ]).

-include("include/mlapi.hrl").

-define(META_VERSION, 1).
-define(MIN_IN_SECS,        60).
-define(HOUR_IN_SECS,     3600).
-define(DAY_IN_SECS,     86400).
-define(WEEK_IN_SECS,   604800).
-define(MONTH_IN_SECS, 2592000).
-define(YEAR_IN_SECS, 31536000).
%% Cache entries are kept for 1 hour by default (this can be overridden per table, see mlapi_metatable).
-define(DEFAULT_CACHE_TTL, ?HOUR_IN_SECS).

-type table_key()       :: any().
-type table_version()   :: non_neg_integer().
-type timestamp()       :: non_neg_integer().
-type last_update()     :: timestamp().
-type time_to_live()    :: non_neg_integer().


-record(mlapi_metatable, {
          table                                             :: mlapi_table(),
          version                                           :: table_version(),
          time_to_live                                      :: time_to_live(),    %% in seconds
          last_update                                       :: calendar:datetime(),
          reason                                            :: any()
         }).

-record(mlapi_cache, {
          key                                               :: any(),
          last_update                                       :: last_update(),
          data                                              :: any()
         }).


-spec init() -> ok | {aborted, Reason :: any()}.
init() ->
    init([node()]).


-spec init([node()]) -> ok | no_return(). %% exit({aborted, Reason :: any()}).
init(Nodes) ->
    init_metatable(Nodes),
    lists:foreach(fun ({Table, Version, _TimeToLive}) -> init_table(Table, Version, Nodes) end, tables()).


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


-spec init_table(mlapi_table(), table_version(), [node()]) -> ok | {aborted, Reason :: any()}.
init_table(Table, NewVersion, Nodes) ->
    Fields = record_info(fields, mlapi_cache),
    OldVersion = case mnesia:dirty_read(mlapi_metatable, Table) of
                     [#mlapi_metatable{version = Version}] ->
                         Version;
                     [] ->
                         NewVersion
                 end,
    %% Make sure that the schema of the Mnesia table is up-to-date.
    try ((OldVersion =:= NewVersion) andalso
         (length(Fields) + 1 =:= mnesia:table_info(Table, arity)) andalso
         (Fields -- mnesia:table_info(Table, attributes)) =:= []) of
        true ->
            ok;
        false ->
            upgrade_table(Table, OldVersion, NewVersion, Fields)
    catch
        _ : _ ->
            create_table(Table, Nodes)
    end.


-spec create_tables([node()]) -> ok | {aborted, Reason :: any()}.
create_tables(Nodes) ->
    create_metatable(Nodes),
    lists:foreach(fun ({Table, _Version}) -> create_table(Table, Nodes) end, tables()).


-spec create_metatable([node()]) -> ok | {aborted, Reason :: any()}.
create_metatable(Nodes) ->
    create_table(mlapi_metatable, mlapi_metatable, record_info(fields, mlapi_metatable), Nodes).


-spec create_table(mlapi_table(), [node()]) -> ok | {aborted, Reason :: any()}.
create_table(Table, Nodes) ->
    create_table(Table, mlapi_cache, record_info(fields, mlapi_cache), Nodes),
    {Version, TimeToLive} = table_info(Table),
    mnesia:dirty_write(mlapi_metatable,
                       #mlapi_metatable{
                         table = Table,
                         version = Version,
                         time_to_live = TimeToLive,
                         last_update = current_time_in_gregorian_seconds(),
                         reason = create_table
                        }).


-spec create_table(mlapi_table(), RecordName :: atom(), [mlapi_field()], [node()]) -> ok | {aborted, Reason :: any()}.
create_table(Table, RecordName, Fields, Nodes) ->
    case mnesia:create_table(Table, [{access_mode, read_write},
                                     {record_name, RecordName},
                                     {attributes, Fields},
                                     {disc_copies, Nodes},
                                     {type, set},
                                     {local_content, true}]) of
        {atomic, ok} ->
            ok;
        Error ->
            Error
    end.


-spec upgrade_metatable() -> {atomic, ok} | {aborted, Reason :: any()}.
upgrade_metatable() ->
    upgrade_table(mlapi_metatable, record_info(fields, mlapi_metatable)).


-spec upgrade_table(mlapi_table()) -> {atomic, ok} | {aborted, Reason :: any()}.
upgrade_table(Table) ->
    upgrade_table(Table, record_info(fields, mlapi_cache)).


-spec upgrade_table(mlapi_table(), [mlapi_field()]) -> {atomic, ok} | {aborted, Reason :: any()}.
upgrade_table(Table, Fields) ->
    %% Replace 'ignore' with a function that performs the schema upgrade once the schema changes.
    mnesia:transform_table(Table, ignore, Fields, Table).


-spec upgrade_table(mlapi_table(), OldVersion :: non_neg_integer(), NewVersion :: non_neg_integer(), [mlapi_field()]) ->
                           {atomic, ok} | {aborted, Reason :: any()}.
upgrade_table(Table, _OldVersion, _NewVersion, Fields) ->
    %% Replace 'ignore' with a function that performs the schema upgrade once the schema changes.
    mnesia:transform_table(Table, ignore, Fields, Table).


-spec tables() -> [mlapi_table()].
tables() ->
    [
     %% Table                 Version  Time-to-live
     {mlapi_cached_list,            1, ?HOUR_IN_SECS},
     {mlapi_site_ext,               1, ?MONTH_IN_SECS},
     {mlapi_country_ext,            1, ?MONTH_IN_SECS},
     {mlapi_state_ext,              1, ?WEEK_IN_SECS},
     {mlapi_city_ext,               1, ?WEEK_IN_SECS},
     {mlapi_currency,               1, ?MONTH_IN_SECS},
     {mlapi_currency_conversion,    1, ?HOUR_IN_SECS},
     {mlapi_listing_exposure,       1, ?WEEK_IN_SECS},
     {mlapi_card_issuer_ext,        1, ?DAY_IN_SECS},
     {mlapi_payment_type,           1, ?DAY_IN_SECS},
     {mlapi_payment_method_ext,     1, ?DAY_IN_SECS},
     {mlapi_category_ext,           1, ?WEEK_IN_SECS},
     {mlapi_user,                   1, ?DAY_IN_SECS},
     {mlapi_item,                   1, 30 * ?MIN_IN_SECS},
     {mlapi_picture,                1, ?WEEK_IN_SECS},
     {mlapi_geolocation,            1, ?WEEK_IN_SECS},
     {mlapi_search_result,          1, 30 * ?MIN_IN_SECS}
    ].


-spec table_info(mlapi_table()) -> {table_version(), time_to_live()} | undefined.
table_info(Table) ->
    case lists:keyfind(Table, 1, tables()) of
        {Table, Version, TimeToLive} ->
            {Version, TimeToLive};
        false ->
            undefined
    end.

-spec table_version(mlapi_table()) -> table_version().
table_version(Table) ->
    case lists:keyfind(Table, 1, tables()) of
        {Table, Version, _TimeToLive} ->
            Version;
        false ->
            undefined
    end.


-spec table_ttl(mlapi_table()) -> time_to_live().
table_ttl(Table) ->
    case lists:keyfind(Table, 1, tables()) of
        {Table, _Version, TimeToLive} ->
            TimeToLive;
        false ->
            mlapi:get_env(cache_ttl)
    end.


-spec sites() -> mlapi:response().
sites() ->
    sites([]).

-spec sites([mlapi:option()]) -> mlapi:response().
sites(Options) ->
    get_data(mlapi_cached_list, mlapi_site, Options, fun (NewOptions) -> mlapi:sites(NewOptions) end).

-spec site(mlapi_site_id()) -> mlapi:response().
site(SiteId) ->
    site(SiteId, []).

-spec site(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
site(SiteId, Options) ->
    get_data(mlapi_site_ext, SiteId, Options, fun (NewOptions) -> mlapi:site(SiteId, NewOptions) end).


-spec countries() -> mlapi:response().
countries() ->
    countries([]).

-spec countries([mlapi:option()]) -> mlapi:response().
countries(Options) ->
    get_data(mlapi_cached_list, mlapi_country, Options, fun (NewOptions) -> mlapi:countries(NewOptions) end).

-spec country(mlapi_country_id()) -> mlapi:response().
country(CountryId) ->
    country(CountryId, []).

-spec country(mlapi_country_id(), [mlapi:option()]) -> mlapi:response().
country(CountryId, Options) ->
    get_data(mlapi_country_ext, CountryId, Options, fun (NewOptions) -> mlapi:country(CountryId, NewOptions) end).


-spec state(mlapi_state_id()) -> mlapi:response().
state(StateId) ->
    state(StateId, []).

-spec state(mlapi_state_id(), [mlapi:option()]) -> mlapi:response().
state(StateId, Options) ->
    get_data(mlapi_state_ext, StateId, Options, fun (NewOptions) -> mlapi:state(StateId, NewOptions) end).


-spec city(mlapi_city_id()) -> mlapi:response().
city(CityId) ->
    city(CityId, []).

-spec city(mlapi_city_id(), [mlapi:option()]) -> mlapi:response().
city(CityId, Options) ->
    get_data(mlapi_city_ext, CityId, Options, fun (NewOptions) -> mlapi:city(CityId, NewOptions) end).


-spec currencies() -> mlapi:response().
currencies() ->
    currencies([]).

-spec currencies([mlapi:option()]) -> mlapi:response().
currencies(Options) ->
    get_data(mlapi_cached_list, mlapi_currency, Options, fun (NewOptions) -> mlapi:currencies(NewOptions) end).


-spec currency(mlapi_currency_id()) -> mlapi:response().
currency(CurrencyId) ->
    currency(CurrencyId, []).

-spec currency(mlapi_currency_id(), [mlapi:option()]) -> mlapi:response().
currency(CurrencyId, Options) ->
    get_data(mlapi_currency, CurrencyId, Options, fun (NewOptions) -> mlapi:currency(CurrencyId, NewOptions) end).

-spec currency_conversion(FromCurrencyId :: mlapi_currency_id(), ToCurrencyId :: mlapi_currency_id()) -> mlapi:response().
currency_conversion(FromCurrencyId, ToCurrencyId) ->
    currency_conversion(FromCurrencyId, ToCurrencyId, []).

-spec currency_conversion(FromCurrencyId :: mlapi_currency_id(), ToCurrencyId :: mlapi_currency_id(),
                              [mlapi:option()] | calendar:datetime()) -> mlapi:response().
currency_conversion(FromCurrencyId, ToCurrencyId, Options) when is_list(Options) ->
    get_data(mlapi_currency_conversion, {FromCurrencyId, ToCurrencyId}, Options,
             fun (NewOptions) -> mlapi:currency_conversion(FromCurrencyId, ToCurrencyId, NewOptions) end);
currency_conversion(FromCurrencyId, ToCurrencyId, {_Date, _Time} = Datetime) ->
    currency_conversion(FromCurrencyId, ToCurrencyId, Datetime, []).

-spec currency_conversion(FromCurrencyId :: mlapi_currency_id(),
                              ToCurrencyId :: mlapi_currency_id(), calendar:datetime(), [mlapi:option()]) -> mlapi:response().
currency_conversion(FromCurrencyId, ToCurrencyId, {Date, {Hour, _Min, _Sec}}, Options) ->
    %% We assume that currency conversions are valid for 1 hour.
    Datetime = {Date, {Hour, 0, 0}},
    get_data(mlapi_currency_conversion, {FromCurrencyId, ToCurrencyId, Datetime}, Options,
             fun (NewOptions) -> mlapi:currency_conversion(FromCurrencyId, ToCurrencyId, Datetime, NewOptions) end).

-spec listing_exposures(mlapi_site_id()) -> mlapi:response().
listing_exposures(SiteId) ->
    listing_exposures(SiteId, []).

-spec listing_exposures(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
listing_exposures(SiteId, Options) ->
    get_data(mlapi_cached_list, {mlapi_listing_exposure, SiteId}, Options,
             fun (NewOptions) -> mlapi:listing_exposures(SiteId, NewOptions) end).


-spec listing_exposure(mlapi_site_id(), mlapi_listing_exposure_id()) -> mlapi:response().
listing_exposure(SiteId, ListingExposureId) ->
    listing_exposure(SiteId, ListingExposureId, []).

-spec listing_exposure(mlapi_site_id(), mlapi_listing_exposure_id(), [mlapi:option()]) -> mlapi:response().
listing_exposure(SiteId, ListingExposureId, Options) ->
    get_data(mlapi_listing_exposure, {SiteId, ListingExposureId}, Options,
             fun (NewOptions) -> mlapi:listing_exposure(SiteId, ListingExposureId, NewOptions) end).


-spec listing_types(mlapi_site_id()) -> mlapi:response().
listing_types(SiteId) ->
    listing_types(SiteId, []).

-spec listing_types(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
listing_types(SiteId, Options) ->
    get_data(mlapi_cached_list, {mlapi_listing_type, SiteId}, Options,
             fun (NewOptions) -> mlapi:listing_types(SiteId, NewOptions) end).


-spec listing_prices(mlapi_site_id()) -> mlapi:response().
listing_prices(SiteId) ->
    listing_prices(SiteId, []).

-spec listing_prices(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
listing_prices(SiteId, Options) ->
    get_data(mlapi_cached_list, {mlapi_listing_price, SiteId}, Options,
             fun (NewOptions) -> mlapi:listing_prices(SiteId, NewOptions) end).


-spec payment_types() -> mlapi:response().
payment_types() ->
    payment_types([]).

-spec payment_types([mlapi:option()]) -> mlapi:response().
payment_types(Options) ->
    get_data(mlapi_cached_list, mlapi_payment_type, Options,
             fun (NewOptions) -> mlapi:payment_types(NewOptions) end).

-spec payment_type(mlapi_payment_type_id()) -> mlapi:response().
payment_type(PaymentTypeId) ->
    payment_type(PaymentTypeId, []).

-spec payment_type(mlapi_payment_type_id(), [mlapi:option()]) -> mlapi:response().
payment_type(PaymentTypeId, Options) ->
    get_data(mlapi_payment_type, PaymentTypeId, Options,
             fun (NewOptions) -> mlapi:payment_type(PaymentTypeId, NewOptions) end).


-spec payment_methods(mlapi_site_id()) -> mlapi:response().
payment_methods(SiteId) ->
    payment_methods(SiteId, []).

-spec payment_methods(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
payment_methods(SiteId, Options) ->
    get_data(mlapi_cached_list, {mlapi_payment_method, SiteId}, Options,
             fun (NewOptions) -> mlapi:payment_methods(SiteId, NewOptions) end).

-spec payment_method(mlapi_site_id(), mlapi_payment_method_id()) -> mlapi:response().
payment_method(SiteId, PaymentMethodId) ->
    payment_method(SiteId, PaymentMethodId, []).

-spec payment_method(mlapi_site_id(), mlapi_payment_method_id(), [mlapi:option()]) -> mlapi:response().
payment_method(SiteId, PaymentMethodId, Options) ->
    get_data(mlapi_payment_method_ext, {SiteId, PaymentMethodId}, Options,
             fun (NewOptions) -> mlapi:payment_method(SiteId, PaymentMethodId, NewOptions) end).


-spec card_issuers(mlapi_site_id()) -> mlapi:response().
card_issuers(SiteId) ->
    card_issuers(SiteId, []).

-spec card_issuers(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
card_issuers(SiteId, Options) ->
    get_data(mlapi_cached_list, {mlapi_card_issuer, SiteId}, Options,
             fun (NewOptions) -> mlapi:card_issuers(SiteId, NewOptions) end).

-spec card_issuer(mlapi_site_id(), mlapi_card_issuer_id()) -> mlapi:response().
card_issuer(SiteId, CardIssuerId) ->
    card_issuer(SiteId, CardIssuerId, []).

-spec card_issuer(mlapi_site_id(), mlapi_card_issuer_id(), [mlapi:option()]) -> mlapi:response().
card_issuer(SiteId, CardIssuerId, Options) ->
    get_data(mlapi_card_issuer_ext, {SiteId, CardIssuerId}, Options,
             fun (NewOptions) -> mlapi:card_issuer(SiteId, CardIssuerId, NewOptions) end).


-spec category(mlapi_category_id()) -> mlapi:response().
category(CategoryId) ->
    category(CategoryId, []).

-spec category(mlapi_category_id(), [mlapi:option()]) -> mlapi:response().
category(CategoryId, Options) ->
    get_data(mlapi_category_ext, CategoryId, Options,
             fun (NewOptions) -> mlapi:category(CategoryId, NewOptions) end).


-spec user(mlapi_user_id()) -> mlapi:response().
user(UserId) ->
    user(UserId, []).

-spec user(mlapi_user_id(), [mlapi:option()]) -> mlapi:response().
user(UserId, Options) ->
    get_data(mlapi_user, UserId, Options, fun (NewOptions) -> mlapi:user(UserId, NewOptions) end).


-spec item(mlapi_item_id()) -> mlapi:response().
item(ItemId) ->
    item(ItemId, []).

-spec item(mlapi_item_id(), [mlapi:option()]) -> mlapi:response().
item(ItemId, Options) ->
    get_data(mlapi_item, ItemId, Options, fun (NewOptions) -> mlapi:item(ItemId, NewOptions) end).


-spec picture(mlapi_picture_id()) -> mlapi:response().
picture(PictureId) ->
    picture(PictureId, []).

-spec picture(mlapi_picture_id(), [mlapi:option()]) -> mlapi:response().
picture(PictureId, Options) ->
    get_data(mlapi_picture, PictureId, Options, fun (NewOptions) -> mlapi:picture(PictureId, NewOptions) end).


-spec trends(mlapi_site_id()) -> mlapi:response().
trends(SiteId) ->
    trends(SiteId, []).

-spec trends(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
trends(SiteId, Options) when is_list(Options) ->
    get_data(mlapi_cached_list, {mlapi_trend, SiteId}, Options,
             fun (NewOptions) -> mlapi:trends(SiteId, NewOptions) end).


-spec category_trends(mlapi_site_id(), mlapi_category_id()) -> mlapi:response().
category_trends(SiteId, CategoryId) ->
    category_trends(SiteId, CategoryId, []).

-spec category_trends(mlapi_site_id(), mlapi_category_id(), [mlapi:option()] | non_neg_integer()) -> mlapi:response().
category_trends(SiteId, CategoryId, Options) when is_list(Options) ->
    get_data(mlapi_cached_list, {mlapi_trend, SiteId, CategoryId}, Options,
             fun (NewOptions) -> mlapi:category_trends(SiteId, CategoryId, NewOptions) end);
category_trends(SiteId, CategoryId, Limit) ->
    category_trends(SiteId, CategoryId, Limit, []).

-spec category_trends(mlapi_site_id(), mlapi_category_id(), Limit :: non_neg_integer(), [mlapi:option()]) -> mlapi:response().
category_trends(SiteId, CategoryId, Limit, Options) ->
    %% Dumb function: this could take advantage of the data cached by the version
    %% of category_trends that does not have a limit.
    get_data(mlapi_cached_list, {mlapi_trend, SiteId, CategoryId, Limit}, Options,
             fun (NewOptions) -> mlapi:category_trends(SiteId, CategoryId, Limit, NewOptions) end).


-spec local_geolocation() -> mlapi:response().
local_geolocation() ->
    local_geolocation([]).

-spec local_geolocation([mlapi:option()]) -> mlapi:response().
local_geolocation(Options) ->
    get_data(mlapi_geolocation, whereami, Options, fun (NewOptions) -> mlapi:local_geolocation(NewOptions) end).

-spec geolocation(mlapi_ip_address()) -> mlapi:response().
geolocation(IpAddr) ->
    geolocation(IpAddr, []).

-spec geolocation(mlapi_ip_address(), [mlapi:option()]) -> mlapi:response().
geolocation(IpAddr, Options) ->
    get_data(mlapi_geolocation, IpAddr, Options, fun (NewOptions) -> mlapi:geolocation(IpAddr, NewOptions) end).


-spec search(mlapi_site_id(), Query :: string()) -> mlapi:response().
search(SiteId, Query) ->
    search(SiteId, Query, []).

-spec search(mlapi_site_id(), Query :: string(), [mlapi:option()]) -> mlapi:response().
search(SiteId, Query, Options) ->
    get_data(mlapi_search_result, {fulltext, SiteId, Query}, Options, fun (NewOptions) -> mlapi:search(SiteId, Query, NewOptions) end).

-spec search(mlapi_site_id(), Query :: string(), Offset :: non_neg_integer(),
             Limit :: non_neg_integer()) -> mlapi:response().
search(SiteId, Query, Offset, Limit) ->
    search(SiteId, Query, Offset, Limit, []).

-spec search(mlapi_site_id(), Query :: string(), Offset :: non_neg_integer(),
             Limit :: non_neg_integer(), [mlapi:option()]) -> mlapi:response().
search(SiteId, Query, Offset, Limit, Options) ->
    get_data(mlapi_search_result, {fulltext, SiteId, Query, Offset, Limit}, Options,
             fun (NewOptions) -> mlapi:search(SiteId, Query, Offset, Limit, NewOptions) end).


-spec search_category(mlapi_site_id(), mlapi_category_id()) -> mlapi:response().
search_category(SiteId, CategoryId) ->
    search_category(SiteId, CategoryId, []).

-spec search_category(mlapi_site_id(), mlapi_category_id(), [mlapi:option()]) -> mlapi:response().
search_category(SiteId, CategoryId, Options) ->
    get_data(mlapi_search_result, {category, SiteId, CategoryId}, Options,
             fun (NewOptions) -> mlapi:search_category(SiteId, CategoryId, NewOptions) end).

-spec search_category(mlapi_site_id(), mlapi_category_id(), Offset :: non_neg_integer(),
                      Limit :: non_neg_integer()) -> mlapi:response().
search_category(SiteId, CategoryId, Offset, Limit) ->
    search_category(SiteId, CategoryId, Offset, Limit, []).

-spec search_category(mlapi_site_id(), mlapi_category_id(), Offset :: non_neg_integer(),
                      Limit :: non_neg_integer(), [mlapi:option()]) -> mlapi:response().
search_category(SiteId, CategoryId, Offset, Limit, Options) ->
    get_data(mlapi_search_result, {category, SiteId, CategoryId, Offset, Limit}, Options,
             fun (NewOptions) -> mlapi:search_category(SiteId, CategoryId, Offset, Limit, NewOptions) end).


-spec search_seller_id(mlapi_site_id(), mlapi_user_id()) -> mlapi:response().
search_seller_id(SiteId, SellerId) ->
    search_seller_id(SiteId, SellerId).

-spec search_seller_id(mlapi_site_id(), mlapi_user_id(), [mlapi:option()]) -> mlapi:response().
search_seller_id(SiteId, SellerId, Options) ->
    get_data(mlapi_search_result, {seller_id, SiteId, SellerId}, Options,
             fun (NewOptions) -> mlapi:search_seller_id(SiteId, SellerId, NewOptions) end).

-spec search_seller_id(mlapi_site_id(), mlapi_user_id(), Offset :: non_neg_integer(),
                       Limit :: non_neg_integer()) -> mlapi:response().
search_seller_id(SiteId, SellerId, Offset, Limit) ->
    search_seller_id(SiteId, SellerId, Offset, Limit, []).

-spec search_seller_id(mlapi_site_id(), mlapi_user_id(), Offset :: non_neg_integer(),
                       Limit :: non_neg_integer(), [mlapi:option()]) -> mlapi:response().
search_seller_id(SiteId, SellerId, Offset, Limit, Options) ->
    get_data(mlapi_search_result, {seller_id, SiteId, SellerId, Offset, Limit}, Options,
             fun (NewOptions) -> mlapi:search_seller_id(SiteId, SellerId, Offset, Limit, NewOptions) end).


-spec search_seller_nick(mlapi_site_id(), mlapi_user_name()) -> mlapi:response().
search_seller_nick(SiteId, Nickname) ->
    search_seller_nick(SiteId, Nickname, []).

-spec search_seller_nick(mlapi_site_id(), mlapi_user_name(), [mlapi:option()]) -> mlapi:response().
search_seller_nick(SiteId, Nickname, Options) ->
    get_data(mlapi_search_result, {seller_nick, SiteId, Nickname}, Options,
             fun (NewOptions) -> mlapi:search_seller_nick(SiteId, Nickname, NewOptions) end).

-spec search_seller_nick(mlapi_site_id(), mlapi_user_name(), Offset :: non_neg_integer(),
                         Limit :: non_neg_integer()) -> mlapi:response().
search_seller_nick(SiteId, Nickname, Offset, Limit) ->
    search_seller_nick(SiteId, Nickname, Offset, Limit, []).

-spec search_seller_nick(mlapi_site_id(), mlapi_user_name(), Offset :: non_neg_integer(),
                         Limit :: non_neg_integer(), [mlapi:option()]) -> mlapi:response().
search_seller_nick(SiteId, Nickname, Offset, Limit, Options) ->
    get_data(mlapi_search_result, {seller_nick, SiteId, Nickname, Offset, Limit}, Options,
             fun (NewOptions) -> mlapi:search_seller_nick(SiteId, Nickname, Offset, Limit, NewOptions) end).


-spec get_data(mlapi_table(), table_key(), [mlapi:option()], RefreshFun :: fun()) -> mlapi:response().
get_data(Table, Key, Options, RefreshFun) ->
    CurrentTime = current_time_in_gregorian_seconds(),
    %% As we cache responses as parsed JSON documents we need to remove the format
    %% from the list of Options and apply it manually before returning the response.
    {Format, NewOptions} = split_format_option(Options),
    Data = case lists:member(refresh, NewOptions) of
               true ->
                   get_fresh_data(Table, Key, NewOptions, RefreshFun, CurrentTime);
               false ->
                   {LastUpdate, CachedData} = cache_entry(Table, Key),
                   %% We store the datetime as seconds in the Gregorian calendar (since Jan 1, 0001 at 00:00:00).
                   case is_cache_valid(Table, LastUpdate, CurrentTime) of
                       true ->
                           CachedData;
                       false ->
                           get_fresh_data(Table, Key, NewOptions, RefreshFun, CurrentTime)
                   end
           end,
    mlapi:json_to_term(Data, record_name(Table, Key), Format).


-spec get_fresh_data(mlapi_table(), table_key(), [mlapi:option()], RefreshFun :: fun(),
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
            %% There's no need to supervise this process; if it fails we'll just refresh the cache.
            WriteFun = fun () ->
                               mnesia:write(Table, #mlapi_cache{key = Key,
                                                                last_update = CurrentTime,
                                                                data = Data
                                                               }, write)
                       end,
            proc_lib:spawn(fun () -> {atomic, _Result} = mnesia:transaction(WriteFun) end),
            Data
    end.


-spec cache_entry(mlapi_table(), table_key()) -> {last_update() | 'undefined', Data :: any() | 'undefined'}.
cache_entry(Table, Key) ->
    case mnesia:dirty_read(Table, Key) of
        [#mlapi_cache{last_update = LastUpdate, data = Data}] ->
            {LastUpdate, Data};
        [] ->
            {undefined, undefined}
    end.


-spec cache_ttl(mlapi_table()) -> time_to_live().
cache_ttl(Table) ->
    case mnesia:dirty_read(mlapi_metatable, Table) of
        [#mlapi_metatable{time_to_live = TimeToLive}] ->
            TimeToLive;
        [] ->
            mlapi:env(cache_ttl, ?DEFAULT_CACHE_TTL)
    end.


-spec is_cache_valid(mlapi_table(), last_update() | 'undefined', timestamp()) -> boolean().
is_cache_valid(_Table, undefined, _CurrentTime) ->
    %% The entry was never updated.
    false;
is_cache_valid(Table, LastUpdate, CurrentTime) ->
    LastUpdate + cache_ttl(Table) > CurrentTime.


-spec split_format_option([mlapi:option()]) -> {mlapi:format(), [mlapi:option()]}.
split_format_option(Options) ->
    case lists:keytake(format, 1, Options) of
        {value, {format, Format}, NewOptions} ->
            {Format, NewOptions};
        false ->
            {json, Options}
    end.


-spec current_time_in_gregorian_seconds() -> non_neg_integer().
current_time_in_gregorian_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).


-spec record_name(mlapi_table(), table_key()) -> RecordName :: atom().
record_name(Table, Key) ->
    case Table of
        mlapi_cached_list ->
            if
                is_atom(Key) ->
                    Key;
                is_tuple(Key) ->
                    element(1, Key)
            end;
        _ ->
            Table
    end.
