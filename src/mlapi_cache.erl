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

-export([init/0, init/1, init_table/2, create_tables/1, create_table/2, create_table/3, upgrade_table/1, tables/0]).
-export([get_sites/0, get_sites/1, get_site/1, get_site/2,
         get_countries/0, get_country/1,
         get_state/1, get_city/1,
         get_currencies/0, get_currency/1,
         get_currency_conversion/2, get_currency_conversion/3,
         get_listing_exposures/1, get_listing_exposure/2,
         %% get_listing_types/1, get_listing_prices/1,
         get_card_issuers/1, get_card_issuer/2,
         get_payment_types/0, get_payment_type/1,
         get_payment_methods/1, get_payment_method/2,
         get_category/1,
         get_user/1,
         get_item/1,
         get_picture/1
        ]).
         %% get_trends/1, get_trends/2, get_category_trends/2, get_category_trends/3, get_category_trends/4
         %get_geolocation/0, get_geolocation/1,
         %search/2, search/4,
         %search_category/2, search_category/4,
         %search_seller_id/2, search_seller_id/4,
         %search_nickname/2, search_nickname/4

-include("include/mlapi.hrl").

%% Cache entries are kept for 1 hour by default (this can be overridden per table, see mlapi_metatable).
-define(META_VERSION, 1).
-define(DEFAULT_CACHE_TTL, 3600).

-type last_update()     :: non_neg_integer().
-type time_to_live()    :: non_neg_integer().


-record(mlapi_metatable, {
          table                                             :: atom(),
          version                                           :: non_neg_integer(),
          time_to_live                                      :: non_neg_integer(),    %% in seconds
          last_update                                       :: calendar:datetime(),
          reason                                            :: any()
         }).

-record(mlapi_cache, {
          key                                               :: any(),
          last_update                                       :: non_neg_integer(),
          data                                              :: any()
         }).


-spec init() -> ok | {aborted, Reason :: any()}.
init() ->
    init([node()]).


-spec init([node()]) -> ok | no_return(). %% exit({aborted, Reason :: any()}).
init(Nodes) ->
    init_metatable(mlapi_metatable, Nodes),
    lists:foreach(fun ({Table, Version}) -> init_table(Table, Version, Nodes) end, tables()).


-spec init_metatable(mlapi_table(), [node()]) -> ok | {aborted, Reason :: any()}.
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


-spec init_table(mlapi_table(), Version :: non_neg_integer(), [node()]) -> ok | {aborted, Reason :: any()}.
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
            create_table(Table, Fields, Nodes)
    end.


-spec create_tables([node()]) -> ok | {aborted, Reason :: any()}.
create_tables(Nodes) ->
    create_metatable(Nodes),
    lists:foreach(fun ({Table, _Version}) -> create_table(Table, Nodes) end, tables()).


-spec create_table([node()]) -> ok | {aborted, Reason :: any()}.
create_metatable(Nodes) ->
    create_table(mlapi_metatable, mlapi_metatable, record_info(fields, mlapi_metatable), Nodes).


-spec create_table(mlapi_table(), [node()]) -> ok | {aborted, Reason :: any()}.
create_table(Table, Nodes) ->
    create_table(Table, mlapi_cache, record_info(fields, mlapi_cache), Nodes).


-spec create_table(mlapi_table(), [mlapi_field()], [node()]) -> ok | {aborted, Reason :: any()}.
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
upgrade_table() ->
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
     {mlapi_list,                   1},
     {mlapi_site_ext,               1},
     {mlapi_country_ext,            1},
     {mlapi_state_ext,              1},
     {mlapi_city_ext,               1},
     {mlapi_currency,               1},
     {mlapi_currency_conversion,    1},
     {mlapi_listing_exposure,       1},
     {mlapi_card_issuer_ext,        1},
     {mlapi_payment_type,           1},
     {mlapi_payment_method_ext,     1},
     {mlapi_category_ext,           1},
     {mlapi_user,                   1},
     {mlapi_item,                   1},
     {mlapi_picture,                1}

     %{mlapi_listing_type,           1},
     %{mlapi_listing_price,          1},
     %{mlapi_card_issuer,            1},
    ].


-spec get_sites() -> mlapi:response().
get_sites() ->
    get_sites([]).

-spec get_sites([mlapi:option()]) -> mlapi:response().
get_sites(Options) ->
    get_data(mlapi_cached_list, mlapi_site, Options, fun (NewOptions) -> mlapi:get_sites(NewOptions) end).

-spec get_site(mlapi_site_id()) -> mlapi:response().
get_site(SiteId) ->
    get_site(SiteId, []).

-spec get_site(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
get_site(SiteId, Options) ->
    get_data(mlapi_site_ext, SiteId, Options, fun (NewOptions) -> mlapi:get_site(SiteId, NewOptions) end).


-spec get_countries() -> mlapi:response().
get_countries() ->
    get_countries([]).

-spec get_countries([mlapi:option()]) -> mlapi:response().
get_countries(Options) ->
    get_data(mlapi_cached_list, mlapi_country, Options, fun (NewOptions) -> mlapi:get_countries(NewOptions) end).

-spec get_country(mlapi_country_id()) -> mlapi:response().
get_country(CountryId) ->
    get_country(CountryId, []).

-spec get_country(mlapi_country_id(), [mlapi:option()]) -> mlapi:response().
get_country(CountryId, Options) ->
    get_data(mlapi_country_ext, CountryId, Options, fun (NewOptions) -> mlapi:get_country(CountryId, NewOptions) end).


-spec get_state(mlapi_state_id()) -> mlapi:response().
get_state(StateId) ->
    get_state(StateId, []).

-spec get_state(mlapi_state_id(), [mlapi:option()]) -> mlapi:response().
get_state(StateId, Options) ->
    get_data(mlapi_state_ext, StateId, Options, fun (NewOptions) -> mlapi:get_state(StateId, NewOptions) end).


-spec get_city(mlapi_city_id()) -> mlapi:response().
get_city(CityId) ->
    get_city(CityId, []).

-spec get_city(mlapi_city_id(), [mlapi:option()]) -> mlapi:response().
get_city(CityId, Options) ->
    get_data(mlapi_city_ext, CityId, Options, fun (NewOptions) -> mlapi:get_city(CityId, NewOptions) end).


get_currencies() ->
    get_currencies([]).

get_currencies(Options) ->
    get_data(mlapi_cached_list, mlapi_currency, Options, fun (NewOptions) -> mlapi:get_currencies(NewOptions) end).


get_currency(CurrencyId) ->
    get_data(mlapi_currency, CurrencyId, fun () -> mlapi:get_currency(CurrencyId) end).

get_currency_conversion(FromCurrencyId, ToCurrencyId) ->
    %% There's no need to cache currency conversions
    get_data(mlapi_currency_conversion, {FromCurrencyId, ToCurrencyId},
             fun () -> mlapi:get_currency_conversion(FromCurrencyId, ToCurrencyId) end).

get_currency_conversion(FromCurrencyId, ToCurrencyId, Datetime) ->
    %% There's no need to cache currency conversions
    mlapi:get_currency_conversion(FromCurrencyId, ToCurrencyId, Datetime).

get_listing_exposures(SiteId) ->
    get_data(mlapi_cached_list, {mlapi_listing_exposure, SiteId}, fun () -> mlapi:get_listing_exposures(SiteId) end).

get_listing_exposure(SiteId, ListingExposureId) ->
    get_data(mlapi_listing_exposure, {SiteId, ListingExposureId}, fun () -> mlapi:get_listing_exposure(SiteId, ListingExposureId) end).

get_card_issuers(SiteId) ->
    get_data(mlapi_cached_list, {mlapi_card_issuer, SiteId}, fun () -> mlapi:get_card_issuers(SiteId) end).

get_card_issuer(SiteId, CardIssuerId) ->
    get_data(mlapi_card_issuer_ext, {SiteId, CardIssuerId}, fun () -> mlapi:get_card_issuer(SiteId, CardIssuerId) end).

get_payment_types() ->
    get_data(mlapi_cached_list, mlapi_payment_type, fun () -> mlapi:get_payment_types() end).

get_payment_type(PaymentTypeId) ->
    get_data(mlapi_payment_type, PaymentTypeId, fun () -> mlapi:get_payment_type(PaymentTypeId) end).

get_payment_methods(SiteId) ->
    get_data(mlapi_cached_list, {mlapi_payment_method, SiteId}, fun () -> mlapi:get_payment_methods(SiteId) end).

get_payment_method(SiteId, PaymentMethodId) ->
    get_data(mlapi_payment_method_ext, {SiteId, PaymentMethodId}, fun () -> mlapi:get_payment_method(SiteId, PaymentMethodId) end).


get_category(CategoryId) ->
    get_data(mlapi_category_ext, CategoryId, fun () -> mlapi:get_category(CategoryId) end).


get_user(UserId) ->
    get_data(mlapi_user, UserId, fun () -> mlapi:get_user(UserId) end).


get_item(ItemId) ->
    get_data(mlapi_item, ItemId, fun () -> mlapi:get_item(ItemId) end).


get_picture(PictureId) ->
    get_data(mlapi_picture, PictureId, fun () -> mlapi:get_picture(PictureId) end).


get_data(Table, Key, Options, RefreshFun) ->
    {LastUpdate, CachedData} = get_cache_entry(Table, Key),
    CurrentTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    case CachedData =:= undefined orelse is_cache_expired(Table, LastUpdate, CurrentTime) of
        true ->
            {Format, NewOptions} = split_format_option(Options),
            case RefreshFun(NewOptions) of
                {error, _Reason} = Error ->
                    Error;
                Data when is_list(Data) ->
                    WriteFun = fun () -> mnesia:write(#mlapi_cache{key = Key,
                                                                   last_update = CurrentTime,
                                                                   data = Data
                                                                  })
                               end,
                    %% Write the entry in a separate process to speed up the response to the caller.
                    proc_lib:spawn(mnesia, transaction, WriteFun),
                    mlapi:json_to_term(Data, Format)
            end;
        false ->
            CachedData
    end.

-spec get_cache_entry(mlapi_table()) -> {last_update(), Data :: any()}.
get_cache_entry(Table, Key) ->
    case mnesia:dirty_read(Table, Key) of
        [#mlapi_cache{last_update = LastUpdate, data = Data}] ->
            {LastUpdate, Data};
        [] ->
            {0, undefined}
    end.

-spec get_cache_ttl(mlapi_table()) -> time_to_live().
get_cache_ttl(Table) ->
    case mnesia:dirty_read(mlapi_metatable, Key) of
        [#mlapi_metatable{time_to_live = TimeToLive}] ->
            TimeToLive;
        [] ->
            mlapi:get_env(cache_ttl, ?DEFAULT_CACHE_TTL)
    end.

is_cache_expired(Table, LastUpdate, CurrentTime) ->
    LastUpdate + get_cache_ttl(Table) > CurrentTime.


split_format_option(Options) ->
    case lists:keytake(format, Options) of
        {value, {format, Format}, NewOptions} ->
            {Format, NewOptions};
        false ->
            {json, Options}
    end.


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


-ifdef(GET_LIST).
get_list(Table, Key, Fun) ->
    case mnesia:dirty_read(Table, Key) of
        CachedList when is_list(CachedList) ->
            lists:map(fun (#mlapi_cached_list{data = Data}) -> Data end, CachedList);
        [] ->
            case Fun() of
                List when is_list(List) ->
                    lists:foreach(fun (Data) ->
                                          mnesia:dirty_write(Table,
                                                             #mlapi_cached_list{key = Key,
                                                                                last_update = calendar:universal_time(),
                                                                                data = Data
                                                                               })
                                  end, List),
                    List;
                Error ->
                    Error
            end
    end.
-endif.

