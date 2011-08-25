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

-export([init/0, init/1, tables/0, create_tables/1, import/1]).
-export([import_currencies/0, import_payment_types/0, import_countries/0,
         import_listing_exposures/1, import_listing_types/1, import_listing_prices/1]).

-include("include/mlapi.hrl").

-type table() :: atom().


-spec init() -> ok | {aborted, Reason :: any()}.
init() ->
    init([node()]).


-spec init([node()]) -> ok | {aborted, Reason :: any()}.
init(Nodes) ->
    try
        mnesia:table_info(mlapi_site, type),
        ok
    catch
        _ : _ ->
            try create_tables(Nodes) of
                {atomic, ok} ->
                    ok;
                Error ->
                    Error
            catch
                _ : Error ->
                    Error
            end
    end.



-spec create_tables([node()]) -> ok | {aborted, Reason :: any()}.
create_tables(Nodes) ->
    lists:foreach(fun (Table) -> create_table(Table, Nodes) end, tables()).


-spec create_table(table(), [node()]) -> {atomic, ok} | {aborted, Reason :: any()}.
create_table(Table, Nodes) ->
    mnesia:create_table(Table, [{access_mode, read_write},
                                {attributes, mlapi:fields(Table)},
                                {disc_copies, Nodes},
                                {type, set},
                                {local_content, true}]).


-spec tables() -> [table()].
tables() ->
    [
     mlapi_last_update,
     mlapi_currency,
     mlapi_listing_exposure,
     mlapi_listing_price,
     mlapi_listing_type,
     mlapi_payment_type,
     mlapi_payment_method,
     mlapi_payment_method_ext,
     mlapi_category,
     mlapi_category_ext,
     mlapi_city,
     mlapi_city_ext,
     mlapi_country,
     mlapi_country_ext,
     mlapi_site,
     mlapi_site_ext,
     mlapi_state,
     mlapi_state_ext
    ].


-spec site_to_country(SiteId :: mlapi_site_id()) -> mlapi_country_id().
site_country(SiteId) ->
    SiteToCountry = [
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
                    ],
    lists:keyfind(SiteId, 1, SiteToCountry).


import(SiteId) ->
    import_currencies(),
    import_payment_types(),
    import_countries(),
    import_sites().


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


import_countries() ->
    case mlapi:get_countries() of
        RawCountries when is_list(RawCountries) ->
            mnesia:clear_table(mlapi_country),
            mnesia:clear_table(mlapi_country_ext),
            mnesia:clear_table(mlapi_state),
            mnesia:clear_table(mlapi_state_ext),
            mnesia:clear_table(mlapi_city),
            mnesia:clear_table(mlapi_city_ext),
            lists:foreach(fun (Country) ->
                                  mnesia:dirty_write(Country),
                                  case mlapi:get_country(Country#mlapi_country.id) of
                                      {Elements} = RawCountryExt when is_list(Elements) ->
                                          CountryExt = mlapi:json_to_record(RawCountryExt, mlapi_country_ext),
                                          mnesia:dirty_write(CountryExt),
                                          import_states(CountryExt#mlapi_country_ext.states);
                                      Error ->
                                          throw(Error)
                                  end
                          end, mlapi:json_to_record(RawCountries, mlapi_country)),
            set_last_update([mlapi_country, mlapi_country_ext,
                             mlapi_state, mlapi_state_ext,
                             mlapi_city, mlapi_city_ext]);
        Error ->
            throw(Error)
    end.


import_states(States) ->
    lists:foreach(fun (State) ->
                          mnesia:dirty_write(State),
                          case mlapi:get_state(State#mlapi_state.id) of
                              {Elements} = RawStateExt when is_list(Elements) ->
                                  StateExt = mlapi:json_to_record(RawStateExt, mlapi_state_ext),
                                  mnesia:dirty_write(StateExt),
                                  import_cities(StateExt#mlapi_state_ext.cities);
                              Error ->
                                  throw(Error)
                          end
                  end, States),
    ok.


import_cities(Cities) ->
    lists:foreach(fun (City) ->
                          mnesia:dirty_write(City),
                          case mlapi:get_city(City#mlapi_city.id) of
                              {Elements} = RawCityExt when is_list(Elements) ->
                                  CityExt = mlapi:json_to_record(RawCityExt, mlapi_city_ext),
                                  mnesia:dirty_write(CityExt);
                              Error ->
                                  throw(Error)
                          end
                  end, Cities),
    ok.

import_sites() ->
    case mlapi:get_sites() of
        RawSites when is_list(RawSites) ->
            mnesia:clear_table(mlapi_site),
            mnesia:clear_table(mlapi_site_ext),
            mnesia:clear_table(mlapi_category),
            mnesia:clear_table(mlapi_category_ext),
            lists:foreach(fun (Site) ->
                                  mnesia:dirty_write(Site),
                                  case mlapi:get_site(Site#mlapi_site.id) of
                                      {Elements} = RawSiteExt when is_list(Elements) ->
                                          SiteExt = mlapi:json_to_record(RawSiteExt, mlapi_site_ext),
                                          mnesia:dirty_write(SiteExt),
                                          import_listing_exposures(Site#mlapi_site.id),
                                          import_listing_types(Site#mlapi_site.id),
                                          import_listing_prices(Site#mlapi_site.id),
                                          import_payment_methods(Site#mlapi_site.id),
                                          import_categories(SiteExt#mlapi_site_ext.categories);
                                      Error ->
                                          throw(Error)
                                  end
                          end, mlapi:json_to_record(RawSites, mlapi_site)),
            set_last_update([mlapi_site, mlapi_site_ext,
                             mlapi_category, mlapi_category_ext]);
        Error ->
            throw(Error)
    end.

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
            set_last_update(mlapi_payment_method),
            set_last_update(mlapi_payment_method_ext);
        Error ->
            throw(Error)
    end.

import_categories(Categories) ->
    lists:foreach(fun (Category) ->
                          mnesia:dirty_write(Category),
                          case mlapi:get_category(Category#mlapi_category.id) of
                              {Elements} = RawCategoryExt when is_list(Elements) ->
                                  CategoryExt = mlapi:json_to_record(RawCategoryExt, mlapi_category_ext),
                                  mnesia:dirty_write(CategoryExt),
                                  import_categories(CategoryExt#mlapi_category_ext.children_categories);
                              Error ->
                                  throw(Error)
                          end
                  end, Categories),
    ok.


set_last_update(Table) when is_atom(Table) ->
    mnesia:dirty_write(#mlapi_last_update{table = Table, timestamp = calendar:universal_time()});
set_last_update(Tables) ->
    Timestamp = calendar:universal_time(),
    lists:foreach(fun (Table) -> mnesia:dirty_write(#mlapi_last_update{table = Table, timestamp = Timestamp}) end, Tables).
