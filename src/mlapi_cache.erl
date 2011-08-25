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
-module(mlapi_cache).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([init/0, init/1, create_tables/1, create_table/2, upgrade_table/1, tables/0]).
-export([import/1, import_currencies/0, import_payment_types/0, import_countries/0, import_sites/0,
         import_site/1, import_country/1, import_state/1, import_city/1,
         import_listing_exposures/1, import_listing_types/1, import_listing_prices/1,
         import_payment_methods/1, import_category/1]).

-include("include/mlapi.hrl").


-spec init() -> ok | {aborted, Reason :: any()}.
init() ->
    init([node()]).


-spec init([node()]) -> ok | no_return(). %% exit({aborted, Reason :: any()}).
init(Nodes) ->
    lists:foreach(fun (Table) -> init_table(Table, Nodes) end, tables()).


-spec init_table(mlapi_table(), [node()]) -> {atomic, ok} | {aborted, Reason :: any()}.
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


-spec create_table(mlapi_table(), [node()]) -> {atomic, ok} | {aborted, Reason :: any()}.
create_table(Table, Nodes) ->
    case mnesia:create_table(Table, [{access_mode, read_write},
                                     {attributes, mlapi:fields(Table)},
                                     {disc_copies, Nodes},
                                     {type, set},
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
     mlapi_last_update,
     mlapi_currency,
     mlapi_payment_type,
     mlapi_country,
     mlapi_country_ext,
     mlapi_state,
     mlapi_state_ext,
     mlapi_city,
     mlapi_city_ext,
     mlapi_site,
     mlapi_site_ext,
     mlapi_listing_exposure,
     mlapi_listing_price,
     mlapi_listing_type,
     mlapi_payment_method,
     mlapi_payment_method_ext,
     mlapi_category,
     mlapi_category_ext
    ].


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
            lists:foreach(fun mnesia:dirty_write/1, ListingExposures),
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
            ListingPrices = mlapi:json_to_record(RawListingPrices, mlapi_listing_price),
            mnesia:clear_table(mlapi_listing_price),
            lists:foreach(fun mnesia:dirty_write/1, ListingPrices),
            set_last_update(mlapi_listing_price);
        Error ->
            throw(Error)
    end.


-spec import_payment_methods(mlapi_site_id()) -> ok.
import_payment_methods(SiteId) ->
    case mlapi:get_payment_methods(SiteId) of
        RawPaymentMethods when is_list(RawPaymentMethods) ->
            mnesia:clear_table(mlapi_payment_method),
            mnesia:clear_table(mlapi_payment_method_ext),
            lists:foreach(fun (RawPaymentMethod) ->
                                  PaymentMethod = mlapi:json_to_record(RawPaymentMethod, mlapi_payment_method),
                                  mnesia:dirty_write(PaymentMethod),
                                  case mlapi:get_payment_method(SiteId, PaymentMethod#mlapi_payment_method.id) of
                                      {Elements} = RawPaymentMethodExt when is_list(Elements) ->
                                          PaymentMethodExt = mlapi:json_to_record(RawPaymentMethodExt, mlapi_payment_method_ext),
                                          mnesia:dirty_write(PaymentMethodExt);
                                      Error ->
                                          throw(Error)
                                  end
                          end, RawPaymentMethods),
            set_last_update([mlapi_payment_method, mlapi_payment_method_ext]);
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
