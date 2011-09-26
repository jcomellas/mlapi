%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc Cache of the contents of a MercadoLibre site that is stored in Mnesia.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(mlapi_scache).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([init/0, init/1, create_tables/1, create_table/2, upgrade_table/1, tables/0]).
-export([import/1, import_currencies/0, import_payment_types/0, import_countries/0, import_sites/0,
         import_site/1, import_country/1, import_state/1, import_city/1,
         import_listing_exposures/1, import_listing_types/1, import_listing_prices/1,
         import_payment_methods/1, import_card_issuers/1, import_category/1, import_categories/2]).
-export([get_sites/0, get_site/1,
         get_countries/0, get_country/1,
         get_state/1, get_city/1, %% get_neighborhood/1,
         get_currencies/0, get_currency/1,
         get_listing_exposures/1, get_listing_exposure/2,
         get_listing_types/1, get_listing_prices/1,
         get_payment_types/0, get_payment_type/1,
         get_payment_methods/1, get_payment_method/2,
         get_card_issuers/1, get_card_issuer/2,
         get_categories/1]).
         %% get_categories/1, get_category/1]).

-include("include/mlapi.hrl").

-spec init() -> ok | {aborted, Reason :: any()}.
init() ->
    init([node()]).


-spec init([node()]) -> ok | no_return(). %% exit({aborted, Reason :: any()}).
init(Nodes) ->
    lists:foreach(fun (Table) -> init_table(Table, Nodes) end, tables()).


-spec init_table(mlapi_table(), [node()]) -> ok | {aborted, Reason :: any()}.
init_table(Table, Nodes) ->
    Fields = mlapi:fields(Table),
    %% Make sure that the schema of each Mnesia table is up-to-date.
    try ((length(Fields) + 1 =:= mnesia:table_info(Table, arity)) andalso
         (Fields -- mnesia:table_info(Table, attributes)) =:= []) of
        true ->
            ok;
        false ->
            upgrade_table(Table)
    catch
        _ : _ ->
            create_table(Table, Nodes)
    end.


-spec create_tables([node()]) -> ok | {aborted, Reason :: any()}.
create_tables(Nodes) ->
    lists:foreach(fun (Table) -> create_table(Table, Nodes) end, tables()).


-spec create_table(mlapi_table(), [node()]) -> ok | {aborted, Reason :: any()}.
create_table(Table, Nodes) ->
    case mnesia:create_table(Table, [{access_mode, read_write},
                                     {attributes, mlapi:fields(Table)},
                                     {disc_copies, Nodes},
                                     {type, table_type(Table)},
                                     {local_content, true}]) of
        {atomic, ok} ->
            ok;
        Error ->
            Error
    end.


-spec upgrade_table(mlapi_table()) -> {atomic, ok} | {aborted, Reason :: any()}.
upgrade_table(Table) ->
    %% Replace 'ignore' with a function that performs the schema upgrade once the schema changes.
    mnesia:transform_table(Table, ignore, mlapi:fields(Table), Table).


-spec tables() -> [mlapi_table()].
tables() ->
    [
     {mlapi_last_update,            1},
     {mlapi_currency,               1},
     {mlapi_payment_type,           1},
     {mlapi_country,                1},
     {mlapi_country_ext,            1},
     {mlapi_state,                  1},
     {mlapi_state_ext,              1},
     {mlapi_city,                   1},
     {mlapi_city_ext,               1},
     {mlapi_site,                   1},
     {mlapi_site_ext,               1},
     {mlapi_listing_exposure,       1},
     {mlapi_listing_type,           1},
     {mlapi_listing_price,          1},
     {mlapi_payment_method,         1},
     {mlapi_payment_method_ext,     1},
     {mlapi_card_issuer,            1},
     {mlapi_card_issuer_ext,        1},
     {mlapi_category,               1},
     {mlapi_category_ext,           1}
    ].


-spec table_type(mlapi_table()) -> set | bag | ordered_set.
table_type(mlapi_listing_exposure) ->
    bag;
table_type(mlapi_listing_type) ->
    bag;
table_type(mlapi_listing_price) ->
    bag;
table_type(mlapi_payment_method) ->
    bag;
table_type(mlapi_payment_method_ext) ->
    bag;
table_type(mlapi_card_issuer) ->
    bag;
table_type(mlapi_card_issuer_ext) ->
    bag;
table_type(_Table) ->
    set.


-spec import(mlapi_site_id()) -> ok.
import(SiteId) ->
    import_currencies(),
    import_payment_types(),
    import_countries(),
    import_sites(),
    import_country(mlapi:site_to_country(SiteId)),
    import_site(SiteId).


-spec import_currencies() -> ok.
import_currencies() ->
    case mlapi:get_currencies() of
        RawCurrencies when is_list(RawCurrencies) ->
            Currencies = mlapi:json_to_record(RawCurrencies, mlapi_currency),
            mnesia:clear_table(mlapi_currency),
            lists:foreach(fun mnesia:dirty_write/1, Currencies),
            set_last_update(mlapi_currency);
        Error ->
            throw(Error)
    end.


-spec import_payment_types() -> ok.
import_payment_types() ->
    case mlapi:get_payment_types() of
        RawPaymentTypes when is_list(RawPaymentTypes) ->
            PaymentTypes = mlapi:json_to_record(RawPaymentTypes, mlapi_payment_type),
            mnesia:clear_table(mlapi_payment_type),
            lists:foreach(fun mnesia:dirty_write/1, PaymentTypes),
            set_last_update(mlapi_payment_type);
        Error ->
            throw(Error)
    end.


-spec import_countries() -> ok.
import_countries() ->
    case mlapi:get_countries() of
        RawCountries when is_list(RawCountries) ->
            Countries = mlapi:json_to_record(RawCountries, mlapi_country),
            mnesia:clear_table(mlapi_country),
            lists:foreach(fun mnesia:dirty_write/1, Countries),
            set_last_update(mlapi_country);
        Error ->
            throw(Error)
    end.


-spec import_sites() -> ok.
import_sites() ->
    case mlapi:get_sites() of
        RawSites when is_list(RawSites) ->
            Sites = mlapi:json_to_record(RawSites, mlapi_site),
            mnesia:clear_table(mlapi_site),
            lists:foreach(fun mnesia:dirty_write/1, Sites),
            set_last_update(mlapi_site);
        Error ->
            throw(Error)
    end.


-spec import_country(mlapi_country_id()) -> ok.
import_country(CountryId) ->
    case mlapi:get_country(CountryId) of
        {Elements} = RawCountryExt when is_list(Elements) ->
            CountryExt = mlapi:json_to_record(RawCountryExt, mlapi_country_ext),
            mnesia:dirty_write(CountryExt),
            set_last_update(mlapi_country_ext, {country, CountryId}),
            import_states(CountryExt#mlapi_country_ext.states, {country, CountryId});
        Error ->
            throw(Error)
    end.


-spec import_states([#mlapi_state{}], Reason :: any()) -> ok.
import_states(States, Reason) ->
    lists:foreach(fun mnesia:dirty_write/1, States),
    set_last_update(mlapi_state, Reason),
    lists:foreach(fun (State) -> import_state(State#mlapi_state.id) end, States).


-spec import_state(mlapi_state_id()) -> ok.
import_state(StateId) ->
    case mlapi:get_state(StateId) of
        {Elements} = RawStateExt when is_list(Elements) ->
            StateExt = mlapi:json_to_record(RawStateExt, mlapi_state_ext),
            mnesia:dirty_write(StateExt),
            Reason = {state, StateId},
            set_last_update(mlapi_state_ext, Reason),
            %% Import cities in current state
            import_cities(StateExt#mlapi_state_ext.cities, Reason);
        Error ->
            throw(Error)
    end.


-spec import_cities([#mlapi_city{}], Reason :: any()) -> ok.
import_cities(Cities, Reason) ->
    lists:foreach(fun mnesia:dirty_write/1, Cities),
    set_last_update(mlapi_city, Reason),
    lists:foreach(fun (City) -> import_city(City#mlapi_city.id) end, Cities).


-spec import_city([mlapi_city_id()]) -> ok.
import_city(CityId) ->
    case mlapi:get_city(CityId) of
        {Elements} = RawCityExt when is_list(Elements) ->
            CityExt = mlapi:json_to_record(RawCityExt, mlapi_city_ext),
            mnesia:dirty_write(CityExt),
            set_last_update(mlapi_city_ext, {city, CityId});
        Error ->
            throw(Error)
    end.


-spec import_site(mlapi_site_id()) -> ok.
import_site(SiteId) ->
    %% mnesia:clear_table(mlapi_site_ext),
    %% mnesia:clear_table(mlapi_category),
    %% mnesia:clear_table(mlapi_category_ext),
    case mlapi:get_site(SiteId) of
        {Elements} = RawSiteExt when is_list(Elements) ->
            SiteExt = mlapi:json_to_record(RawSiteExt, mlapi_site_ext),
            mnesia:dirty_write(SiteExt),
            set_last_update(mlapi_site_ext),
            import_listing_exposures(SiteId),
            import_listing_types(SiteId),
            import_listing_prices(SiteId),
            import_payment_methods(SiteId),
            import_card_issuers(SiteId),
            import_categories(SiteExt#mlapi_site_ext.categories, {site, SiteId}),
            set_last_update([mlapi_category, mlapi_category_ext]);
        Error ->
            throw(Error)
    end.


-spec import_listing_exposures(mlapi_site_id()) -> ok.
import_listing_exposures(SiteId) ->
    case mlapi:get_listing_exposures(SiteId) of
        RawListingExposures when is_list(RawListingExposures) ->
            ListingExposures = mlapi:json_to_record(RawListingExposures, mlapi_listing_exposure),
            mnesia:clear_table(mlapi_listing_exposure),
            lists:foreach(fun (ListingExposure) ->
                                  %% Add missing site_id (not returned by API) to store the record in Mnesia
                                  %% (the mlapi_listing_exposure table is a bag).
                                  mnesia:dirty_write(ListingExposure#mlapi_listing_exposure{site_id = SiteId}) end,
                          ListingExposures),
            set_last_update(mlapi_listing_exposure);
        Error ->
            throw(Error)
    end.


-spec import_listing_types(mlapi_site_id()) -> ok.
import_listing_types(SiteId) ->
    case mlapi:get_listing_types(SiteId) of
        RawListingTypes when is_list(RawListingTypes) ->
            ListingTypes = mlapi:json_to_record(RawListingTypes, mlapi_listing_type),
            mnesia:clear_table(mlapi_listing_type),
            lists:foreach(fun mnesia:dirty_write/1, ListingTypes),
            set_last_update(mlapi_listing_type);
        Error ->
            throw(Error)
    end.


-spec import_listing_prices(mlapi_site_id()) -> ok.
import_listing_prices(SiteId) ->
    case mlapi:get_listing_prices(SiteId) of
        RawListingPrices when is_list(RawListingPrices) ->
            %% Delete all the previous entries for this site.
            mnesia:dirty_delete(mlapi_payment_method, SiteId),
            lists:foreach(fun (RawListingPrice) ->
                                  ListingPrice = mlapi:json_to_record(RawListingPrice, mlapi_listing_price),
                                  %% Add site_id to be able to group the listing prices for the same site together.
                                  mnesia:dirty_write(ListingPrice#mlapi_listing_price{site_id = SiteId})
                          end, RawListingPrices),
            set_last_update(mlapi_listing_price, {site, SiteId});
        Error ->
            throw(Error)
    end.


-spec import_payment_methods(mlapi_site_id()) -> ok.
import_payment_methods(SiteId) ->
    case mlapi:get_payment_methods(SiteId) of
        RawPaymentMethods when is_list(RawPaymentMethods) ->
            %% Delete all the previous entries for this site.
            mnesia:dirty_delete(mlapi_payment_method, SiteId),
            lists:foreach(fun (RawPaymentMethod) ->
                                  PaymentMethod = mlapi:json_to_record(RawPaymentMethod, mlapi_payment_method),
                                  %% Add site_id to be able to group the payment methods for the same site together.
                                  mnesia:dirty_write(PaymentMethod#mlapi_payment_method{site_id = SiteId}),
                                  case mlapi:get_payment_method(SiteId, PaymentMethod#mlapi_payment_method.id) of
                                      {Elements} = RawPaymentMethodExt when is_list(Elements) ->
                                          PaymentMethodExt = mlapi:json_to_record(RawPaymentMethodExt, mlapi_payment_method_ext),
                                          mnesia:dirty_write(PaymentMethodExt);
                                      Error ->
                                          throw(Error)
                                  end
                          end, RawPaymentMethods),
            set_last_update([mlapi_payment_method, mlapi_payment_method_ext], {site, SiteId});
        Error ->
            throw(Error)
    end.


-spec import_card_issuers(mlapi_site_id()) -> ok.
import_card_issuers(SiteId) ->
    case mlapi:get_card_issuers(SiteId) of
        RawCardIssuers when is_list(RawCardIssuers) ->
            %% Delete all the previous entries for this site.
            mnesia:dirty_delete(mlapi_card_issuer, SiteId),
            lists:foreach(fun (RawCardIssuer) ->
                                  CardIssuer = mlapi:json_to_record(RawCardIssuer, mlapi_card_issuer),
                                  %% Add site_id to be able to group the card issuers for the same site together.
                                  mnesia:dirty_write(CardIssuer#mlapi_card_issuer{site_id = SiteId}),
                                  case mlapi:get_card_issuer(SiteId, CardIssuer#mlapi_card_issuer.id) of
                                      {Elements} = RawCardIssuerExt when is_list(Elements) ->
                                          CardIssuerExt = mlapi:json_to_record(RawCardIssuerExt, mlapi_card_issuer_ext),
                                          mnesia:dirty_write(CardIssuerExt);
                                      Error ->
                                          throw(Error)
                                  end
                          end, RawCardIssuers),
            set_last_update([mlapi_payment_method, mlapi_payment_method_ext], {site, SiteId});
        Error ->
            throw(Error)
    end.


-spec import_categories([#mlapi_category{}], Reason :: any()) -> ok.
import_categories(Categories, Reason) ->
    lists:foreach(fun mnesia:dirty_write/1, Categories),
    set_last_update(mlapi_category, Reason),
    lists:foreach(fun (Category) -> import_category(Category#mlapi_category.id) end, Categories).


-spec import_category(mlapi_category_id()) -> ok.
import_category(CategoryId) ->
    case mlapi:get_category(CategoryId) of
        {Elements} = RawCategoryExt when is_list(Elements) ->
            CategoryExt = mlapi:json_to_record(RawCategoryExt, mlapi_category_ext),
            mnesia:dirty_write(CategoryExt),
            Reason = {category, CategoryId},
            set_last_update(mlapi_category_ext, Reason),
            import_categories(CategoryExt#mlapi_category_ext.children_categories, Reason);
        Error ->
            throw(Error)
    end.


-spec set_last_update(mlapi_table() | [mlapi_table()]) -> ok.
set_last_update(Table) ->
    set_last_update(Table, all).


-spec set_last_update(mlapi_table() | [mlapi_table()], Reason :: any()) -> ok.
set_last_update(Table, Reason) when is_atom(Table) ->
    mnesia:dirty_write(#mlapi_last_update{
                          table = Table,
                          timestamp = calendar:universal_time(),
                          reason = Reason
                         });
set_last_update(Tables, Reason) when is_list(Tables) ->
    Timestamp = calendar:universal_time(),
    lists:foreach(fun (Table) ->
                          mnesia:dirty_write(#mlapi_last_update{
                                                table = Table,
                                                timestamp = Timestamp,
                                                reason = Reason
                                               })
                  end, Tables).


-spec get_sites() -> [#mlapi_site{}].
get_sites() ->
    ets:tab2list(mlapi_site).


-spec get_site(mlapi_site_id()) -> #mlapi_site_ext{} | undefined.
get_site(SiteId) ->
    read_single(mlapi_site_ext, SiteId).


-spec get_countries() -> [#mlapi_country{}].
get_countries() ->
    ets:tab2list(mlapi_country).


-spec get_country(mlapi_country_id()) -> #mlapi_country_ext{} | undefined.
get_country(CountryId) ->
    read_single(mlapi_country_ext, CountryId).


-spec get_state(mlapi_state_id()) -> #mlapi_state_ext{} | undefined.
get_state(StateId) ->
    read_single(mlapi_state_ext, StateId).


-spec get_city(mlapi_city_id()) -> #mlapi_city_ext{} | undefined.
get_city(CityId) ->
    read_single(mlapi_city_ext, CityId).


-spec get_currencies() -> [#mlapi_currency{}].
get_currencies() ->
    ets:tab2list(mlapi_currency).


-spec get_currency(mlapi_currency_id()) -> #mlapi_currency{} | undefined.
get_currency(CurrencyId) ->
    read_single(mlapi_currency, CurrencyId).


-spec get_listing_exposures(mlapi_site_id()) -> [#mlapi_listing_exposure{}].
get_listing_exposures(SiteId) ->
    read_multi(mlapi_listing_exposure, SiteId).

-spec get_listing_exposure(mlapi_site_id(), mlapi_listing_exposure_id()) -> #mlapi_listing_exposure{} | undefined.
get_listing_exposure(SiteId, ListingExposureId) ->
    match_single(#mlapi_listing_exposure{site_id = SiteId, id = ListingExposureId, _ = '_'}).


get_listing_types(SiteId) ->
    read_multi(mlapi_listing_type, SiteId).


get_listing_prices(SiteId) ->
    read_multi(mlapi_listing_price, SiteId).


-spec get_payment_types() -> [#mlapi_payment_type{}].
get_payment_types() ->
    ets:tab2list(mlapi_payment_type).


-spec get_payment_type(mlapi_payment_type_id()) -> #mlapi_payment_type{} | undefined.
get_payment_type(PaymentTypeId) ->
    read_single(mlapi_payment_type, PaymentTypeId).


get_payment_methods(SiteId) ->
    read_multi(mlapi_payment_method, SiteId).

get_payment_method(SiteId, PaymentMethodId) ->
    match_single(#mlapi_payment_method_ext{site_id = SiteId, id = PaymentMethodId, _ = '_'}).


-spec get_card_issuers(mlapi_site_id()) -> [#mlapi_card_issuer{}].
get_card_issuers(SiteId) ->
    read_multi(mlapi_card_issuer, SiteId).

-spec get_card_issuer(mlapi_site_id(), mlapi_card_issuer_id()) -> #mlapi_card_issuer{} | undefined.
get_card_issuer(SiteId, CardIssuerId) ->
    match_single(#mlapi_card_issuer{site_id = SiteId, id = CardIssuerId, _ = '_'}).


%%get_subcategories/1, get_category/1]).

-spec get_categories(mlapi_site_id()) -> [#mlapi_category{}].
get_categories(SiteId) ->
    case get_site(SiteId) of
        #mlapi_site_ext{categories = Categories} ->
            Categories;
        Error ->
            Error
    end.


read_single(Table, Key) ->
    case mnesia:dirty_read(Table, Key) of
        [Result] ->
            Result;
        [] ->
            undefined;
        Error ->
            Error
    end.


read_multi(Table, Key) ->
    case mnesia:dirty_read(Table, Key) of
        [_Head | _Tail] = Result ->
            Result;
        Error ->
            Error
    end.


match_single(Record) ->
    case mnesia:dirty_match_object(Record) of
        [Result] ->
            Result;
        [] ->
            undefined;
        Error ->
            Error
    end.
