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
         upgrade_metatable/0, upgrade_table/1, tables/0]).
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
         get_picture/1, get_picture/2
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

-type timestamp()       :: non_neg_integer().
-type last_update()     :: timestamp().
-type time_to_live()    :: non_neg_integer().


-record(mlapi_metatable, {
          table                                             :: atom(),
          version                                           :: non_neg_integer(),
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
    lists:foreach(fun ({Table, Version}) -> init_table(Table, Version, Nodes) end, tables()).


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
    create_table(Table, mlapi_cache, record_info(fields, mlapi_cache), Nodes).


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
     {mlapi_cached_list,            1},
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


-spec get_currencies() -> mlapi:response().
get_currencies() ->
    get_currencies([]).

-spec get_currencies([mlapi:option()]) -> mlapi:response().
get_currencies(Options) ->
    get_data(mlapi_cached_list, mlapi_currency, Options, fun (NewOptions) -> mlapi:get_currencies(NewOptions) end).


-spec get_currency(mlapi_currency_id()) -> mlapi:response().
get_currency(CurrencyId) ->
    get_currency(CurrencyId, []).

-spec get_currency(mlapi_currency_id(), [mlapi:option()]) -> mlapi:response().
get_currency(CurrencyId, Options) ->
    get_data(mlapi_currency, CurrencyId, Options, fun (NewOptions) -> mlapi:get_currency(CurrencyId, NewOptions) end).

-spec get_currency_conversion(FromCurrencyId :: mlapi_currency_id(), ToCurrencyId :: mlapi_currency_id()) -> mlapi:response().
get_currency_conversion(FromCurrencyId, ToCurrencyId) ->
    get_currency_conversion(FromCurrencyId, ToCurrencyId, []).

-spec get_currency_conversion(FromCurrencyId :: mlapi_currency_id(), ToCurrencyId :: mlapi_currency_id(),
                              [mlapi:option()] | calendar:datetime()) -> mlapi:response().
get_currency_conversion(FromCurrencyId, ToCurrencyId, Options) when is_list(Options) ->
    get_data(mlapi_currency_conversion, {FromCurrencyId, ToCurrencyId}, Options,
             fun (NewOptions) -> mlapi:get_currency_conversion(FromCurrencyId, ToCurrencyId, NewOptions) end);
get_currency_conversion(FromCurrencyId, ToCurrencyId, {_Date, _Time} = Datetime) ->
    get_currency_conversion(FromCurrencyId, ToCurrencyId, Datetime, []).

-spec get_currency_conversion(FromCurrencyId :: mlapi_currency_id(),
                              ToCurrencyId :: mlapi_currency_id(), calendar:datetime(), [mlapi:option()]) -> mlapi:response().
get_currency_conversion(FromCurrencyId, ToCurrencyId, {Date, {Hour, _Min, _Sec}}, Options) ->
    %% We assume that currency conversions are valid for 1 hour.
    Datetime = {Date, {Hour, 0, 0}},
    get_data(mlapi_currency_conversion, {FromCurrencyId, ToCurrencyId, Datetime}, Options,
             fun (NewOptions) -> mlapi:get_currency_conversion(FromCurrencyId, ToCurrencyId, Datetime, NewOptions) end).

-spec get_listing_exposures(mlapi_site_id()) -> mlapi:response().
get_listing_exposures(SiteId) ->
    get_listing_exposures(SiteId, []).

-spec get_listing_exposures(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
get_listing_exposures(SiteId, Options) ->
    get_data(mlapi_cached_list, {mlapi_listing_exposure, SiteId}, Options,
             fun (NewOptions) -> mlapi:get_listing_exposures(SiteId, NewOptions) end).


-spec get_listing_exposure(mlapi_site_id(), mlapi_listing_exposure_id()) -> mlapi:response().
get_listing_exposure(SiteId, ListingExposureId) ->
    get_listing_exposure(SiteId, ListingExposureId, []).

-spec get_listing_exposure(mlapi_site_id(), mlapi_listing_exposure_id(), [mlapi:option()]) -> mlapi:response().
get_listing_exposure(SiteId, ListingExposureId, Options) ->
    get_data(mlapi_listing_exposure, {SiteId, ListingExposureId}, Options,
             fun (NewOptions) -> mlapi:get_listing_exposure(SiteId, ListingExposureId, NewOptions) end).


-spec get_listing_types(mlapi_site_id()) -> mlapi:response().
get_listing_types(SiteId) ->
    get_listing_types(SiteId, []).

-spec get_listing_types(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
get_listing_types(SiteId, Options) ->
    get_data(mlapi_cached_list, {mlapi_listing_type, SiteId}, Options,
             fun (NewOptions) -> mlapi:get_listing_types(SiteId, NewOptions) end).


-spec get_listing_prices(mlapi_site_id()) -> mlapi:response().
get_listing_prices(SiteId) ->
    get_listing_prices(SiteId, []).

-spec get_listing_prices(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
get_listing_prices(SiteId, Options) ->
    get_data(mlapi_cached_list, {mlapi_listing_price, SiteId}, Options,
             fun (NewOptions) -> mlapi:get_listing_prices(SiteId, NewOptions) end).


-spec get_payment_types() -> mlapi:response().
get_payment_types() ->
    get_payment_types([]).

-spec get_payment_types([mlapi:option()]) -> mlapi:response().
get_payment_types(Options) ->
    get_data(mlapi_cached_list, mlapi_payment_type, Options,
             fun (NewOptions) -> mlapi:get_payment_types(NewOptions) end).

-spec get_payment_type(mlapi_payment_type_id()) -> mlapi:response().
get_payment_type(PaymentTypeId) ->
    get_payment_type(PaymentTypeId, []).

-spec get_payment_type(mlapi_payment_type_id(), [mlapi:option()]) -> mlapi:response().
get_payment_type(PaymentTypeId, Options) ->
    get_data(mlapi_payment_type, PaymentTypeId, Options,
             fun (NewOptions) -> mlapi:get_payment_type(PaymentTypeId, NewOptions) end).


-spec get_payment_methods(mlapi_site_id()) -> mlapi:response().
get_payment_methods(SiteId) ->
    get_payment_methods(SiteId, []).

-spec get_payment_methods(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
get_payment_methods(SiteId, Options) ->
    get_data(mlapi_cached_list, {mlapi_payment_method, SiteId}, Options,
             fun (NewOptions) -> mlapi:get_payment_methods(SiteId, NewOptions) end).

-spec get_payment_method(mlapi_site_id(), mlapi_payment_method_id()) -> mlapi:response().
get_payment_method(SiteId, PaymentMethodId) ->
    get_payment_method(SiteId, PaymentMethodId, []).

-spec get_payment_method(mlapi_site_id(), mlapi_payment_method_id(), [mlapi:option()]) -> mlapi:response().
get_payment_method(SiteId, PaymentMethodId, Options) ->
    get_data(mlapi_payment_method_ext, {SiteId, PaymentMethodId}, Options,
             fun (NewOptions) -> mlapi:get_payment_method(SiteId, PaymentMethodId, NewOptions) end).


-spec get_card_issuers(mlapi_site_id()) -> mlapi:response().
get_card_issuers(SiteId) ->
    get_card_issuers(SiteId, []).

-spec get_card_issuers(mlapi_site_id(), [mlapi:option()]) -> mlapi:response().
get_card_issuers(SiteId, Options) ->
    get_data(mlapi_cached_list, {mlapi_card_issuer, SiteId}, Options,
             fun (NewOptions) -> mlapi:get_card_issuers(SiteId, NewOptions) end).

-spec get_card_issuer(mlapi_site_id(), mlapi_card_issuer_id()) -> mlapi:response().
get_card_issuer(SiteId, CardIssuerId) ->
    get_card_issuer(SiteId, CardIssuerId, []).

-spec get_card_issuer(mlapi_site_id(), mlapi_card_issuer_id(), [mlapi:option()]) -> mlapi:response().
get_card_issuer(SiteId, CardIssuerId, Options) ->
    get_data(mlapi_card_issuer_ext, {SiteId, CardIssuerId}, Options,
             fun (NewOptions) -> mlapi:get_card_issuer(SiteId, CardIssuerId, NewOptions) end).


-spec get_category(mlapi_category_id()) -> mlapi:response().
get_category(CategoryId) ->
    get_category(CategoryId, []).

-spec get_category(mlapi_category_id(), [mlapi:option()]) -> mlapi:response().
get_category(CategoryId, Options) ->
    get_data(mlapi_category_ext, CategoryId, Options,
             fun (NewOptions) -> mlapi:get_category(CategoryId, NewOptions) end).


-spec get_user(mlapi_user_id()) -> mlapi:response().
get_user(UserId) ->
    get_user(UserId, []).

-spec get_user(mlapi_user_id(), [mlapi:option()]) -> mlapi:response().
get_user(UserId, Options) ->
    get_data(mlapi_user, UserId, Options, fun (NewOptions) -> mlapi:get_user(UserId, NewOptions) end).


-spec get_item(mlapi_item_id()) -> mlapi:response().
get_item(ItemId) ->
    get_item(ItemId, []).

-spec get_item(mlapi_item_id(), [mlapi:option()]) -> mlapi:response().
get_item(ItemId, Options) ->
    get_data(mlapi_item, ItemId, Options, fun (NewOptions) -> mlapi:get_item(ItemId, NewOptions) end).


-spec get_picture(mlapi_picture_id()) -> mlapi:response().
get_picture(PictureId) ->
    get_picture(PictureId, []).

-spec get_picture(mlapi_picture_id(), [mlapi:option()]) -> mlapi:response().
get_picture(PictureId, Options) ->
    get_data(mlapi_picture, PictureId, Options, fun (NewOptions) -> mlapi:get_picture(PictureId, NewOptions) end).

-spec get_data(mlapi_table(), Key :: any(), [mlapi:option()], RefreshFun :: fun()) -> mlapi:response().
get_data(Table, Key, Options, RefreshFun) ->
    {LastUpdate, CachedData} = get_cache_entry(Table, Key),
    %% We store the datetime as seconds since Jan 1, 0001 at 00:00:00.
    CurrentTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    case is_cache_expired(Table, LastUpdate, CurrentTime) of
        true ->
            %% As we cache responses as parsed JSON documents we need to remove the format
            %% from the list of Options and apply it manually before returning the response.
            {Format, NewOptions} = split_format_option(Options),
            case RefreshFun(NewOptions) of
                {error, _Reason} = Error ->
                    Error;
                Data ->
                    WriteFun = fun () ->
                                       mnesia:write(#mlapi_cache{key = Key,
                                                                 last_update = CurrentTime,
                                                                 data = Data
                                                                })
                               end,
                    %% Write the entry in a separate process to speed up the response to the caller.
                    %% There's no need to supervise this process; if it fails we'll just refresh the cache.
                    proc_lib:spawn(mnesia, transaction, WriteFun),
                    mlapi:json_to_term(Data, Format)
            end;
        false ->
            CachedData
    end.


-spec get_cache_entry(mlapi_table(), Key :: any()) -> {last_update() | 'undefined', Data :: any() | 'undefined'}.
get_cache_entry(Table, Key) ->
    case mnesia:dirty_read(Table, Key) of
        [#mlapi_cache{last_update = LastUpdate, data = Data}] ->
            {LastUpdate, Data};
        [] ->
            {undefined, undefined}
    end.


-spec get_cache_ttl(mlapi_table()) -> time_to_live().
get_cache_ttl(Table) ->
    case mnesia:dirty_read(mlapi_metatable, Table) of
        [#mlapi_metatable{time_to_live = TimeToLive}] ->
            TimeToLive;
        [] ->
            mlapi:get_env(cache_ttl, ?DEFAULT_CACHE_TTL)
    end.


-spec is_cache_expired(mlapi_table(), last_update() | 'undefined', timestamp()) -> boolean().
is_cache_expired(_Table, undefined, _CurrentTime) ->
    %% The entry was never updated.
    true;
is_cache_expired(Table, LastUpdate, CurrentTime) ->
    LastUpdate + get_cache_ttl(Table) > CurrentTime.


-spec split_format_option([mlapi:option()]) -> {mlapi:format(), [mlapi:option()]}.
split_format_option(Options) ->
    case lists:keytake(format, Options) of
        {value, {format, Format}, NewOptions} ->
            {Format, NewOptions};
        false ->
            {json, Options}
    end.


%% record_name(Table, Key) ->
%%     case Table of
%%         mlapi_cached_list ->
%%             if
%%                 is_atom(Key) ->
%%                     Key;
%%                 is_tuple(Key) ->
%%                     element(1, Key)
%%             end;
%%         _ ->
%%             Table
%%     end.
