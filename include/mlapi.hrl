%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc MercadoLibre API records.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-ifndef(MLAPI_HRL).
-define(MLAPI_HRL, "mlapi.hrl").

-type mlapi_table()                   :: atom().
-type mlapi_field()                   :: atom().

-type mlapi_id()                      :: binary().

-type mlapi_address_id()              :: mlapi_id().
-type mlapi_attribute_id()            :: mlapi_id().
-type mlapi_card_issuer_id()          :: mlapi_id().
-type mlapi_category_id()             :: mlapi_id().
-type mlapi_city_id()                 :: mlapi_id().
-type mlapi_country_id()              :: mlapi_id().
-type mlapi_currency_id()             :: mlapi_id().
-type mlapi_description_id()          :: mlapi_id().
-type mlapi_item_id()                 :: mlapi_id().
-type mlapi_locale_id()               :: mlapi_id().
-type mlapi_neighborhood_id()         :: mlapi_id().
-type mlapi_payment_method_id()       :: mlapi_id().
-type mlapi_payment_type_id()         :: mlapi_id().
-type mlapi_site_id()                 :: mlapi_id().
-type mlapi_state_id()                :: mlapi_id().
-type mlapi_timezone_id()             :: mlapi_id().
-type mlapi_user_id()                 :: mlapi_id().
-type mlapi_picture_id()              :: mlapi_id().

-type mlapi_url()                     :: binary().
-type mlapi_email_address()           :: binary().
-type mlapi_ip_address()              :: binary().

-type mlapi_required()                :: binary().          %% <<"required">> | <<"optional">>
-type mlapi_buying_mode_id()          :: binary().          %% <<"buy_it_now">> | <<"auction">>
-type mlapi_listing_type_id()         :: binary().          %% <<"gold_premium">> | <<"gold">> | <<"silver">> | <<"bronze">> | <<"free">>
-type mlapi_listing_exposure_id()     :: binary().          %% <<"highest">> | <<"high">> | <<"mid">> | <<"low">> | <<"lowest">>
-type mlapi_item_condition_id()       :: binary().          %% <<"not_specified">> | <<"new">> | <<"used">>
-type mlapi_sale_fees_mode_id()       :: binary().          %% <<"not_free">>
-type mlapi_site_status_id()          :: binary().          %% <<"active">>, <<"pending">>, <<"deactive">>
-type mlapi_item_status_id()          :: binary().          %% <<"not_yet_active">> | <<"paused">> | <<"active">> |
                                                            %% <<"closed">> | <<"deleted">> | <<"invisible">> |
                                                            %% <<"under_review">> | <<"suspended_by_user">>
-type mlapi_user_type_id()            :: binary().          %% <<"car_dealer">>, <<"real_estate_agency">>,
                                                            %% <<"branch">>, <<"franchise">>, <<"normal">>
-type mlapi_seller_experience_id()    :: binary().          %% <<"newbie">>, <<"intermediate">>, <<"advanced">>
-type mlapi_seller_level_id()         :: binary().          %% <<"1_red">>, <<"2_orange">>, <<"3_yellow">>, <<"4_light_green">>, <<"5_green">>
-type mlapi_power_seller_status_id()  :: binary().          %% <<"silver">>, <<"gold">>, <<"platinum">>, <<"null">>
-type mlapi_period_id()               :: binary().          %% <<"historic">>, <<"12 months">>, <<"3 months">>


-record(mlapi_last_update, {
          table                                             :: atom(),
          timestamp                                         :: calendar:datetime(),
          reason                                            :: any()
         }).

-record(mlapi_settings, {
          adult_content                                     :: boolean(),
          buying_allowed                                    :: boolean(),
          buying_modes = []                                 :: [mlapi_buying_mode_id()],
          coverage_areas                                    :: binary(),    %% <<"not_allowed">>,
          immediate_payment                                 :: mlapi_required(),
          item_conditions = []                              :: [mlapi_item_condition_id()],
          items_reviews_allowed                             :: boolean(),
          listing_allowed                                   :: boolean(),
          max_pictures_per_item                             :: integer(),
          mirror_category,
          price                                             :: mlapi_required(),
          seller_contact                                    :: binary(),
          shipping_profile                                  :: mlapi_required(),
          show_contact_information                          :: boolean(),
          simple_shipping                                   :: mlapi_required(),
          stock                                             :: mlapi_required(),
          tags = [],
          vip_subdomain
         }).

-record(mlapi_currency, {
          id                                                :: mlapi_currency_id(),
          description                                       :: binary(),
          symbol                                            :: binary(),
          decimal_places = 2                                :: integer()
         }).

-record(mlapi_currency_conversion, {
          ratio                                             :: float(),
          mercado_pago_ratio                                :: float()
         }).

-record(mlapi_listing_type, {
          site_id                                           :: mlapi_site_id(),
          id                                                :: mlapi_listing_type_id(),
          name                                              :: binary()
         }).

-record(mlapi_listing_exposure, {
          site_id                                           :: mlapi_site_id(),
          id                                                :: mlapi_listing_exposure_id(),
          name                                              :: binary(),
          home_page                                         :: boolean(),
          category_home_page                                :: boolean(),
          advertising_on_listing_page                       :: boolean(),
          priority_in_search                                :: integer()
         }).

-record(mlapi_listing_price, {
          %% site_id added to be able to store the record in Mnesia
          site_id                                           :: mlapi_site_id(),
          listing_type_id                                   :: mlapi_listing_type_id(),
          listing_type_name                                 :: binary(),
          listing_exposure                                  :: mlapi_listing_exposure_id(),
          requires_picture                                  :: boolean(),
          currency_id                                       :: mlapi_currency_id(),
          listing_fee_amount                                :: float(),                     %% fixed amount
          sale_fee_amount                                   :: float()                      %% percentage of sale price
         }).

-record(mlapi_category, {
          id                                                :: mlapi_category_id(),
          name                                              :: binary(),
          total_items_in_this_category                      :: integer()
         }).

-record(mlapi_category_ext, {
          id                                                :: mlapi_category_id(),
          name                                              :: binary(),
          permalink,
          total_items_in_this_category                      :: integer(),
          path_from_root = []                               :: [#mlapi_category{}],
          children_categories = []                          :: [#mlapi_category{}],
          settings                                          :: #mlapi_settings{}
         }).

-record(mlapi_site, {
          id                                                :: mlapi_site_id(),
          name                                              :: binary()
         }).

-record(mlapi_site_ext, {
          id                                                :: mlapi_site_id(),
          name                                              :: binary(),
          country_id                                        :: mlapi_country_id(),
          sale_fees_mode                                    :: mlapi_sale_fees_mode_id(),
          mercadopago_version = 3                           :: non_neg_integer(),
          default_currency_id                               :: mlapi_currency_id(),
          currencies = []                                   :: [#mlapi_currency{}],
          immediate_payment                                 :: mlapi_required(),
          payment_method_ids = []                           :: [mlapi_payment_method_id()],
          categories = []                                   :: [#mlapi_category{}]
         }).

-record(mlapi_location, {
          latitude                                          :: float(),
          longitude                                         :: float()
         }).

-record(mlapi_geo_information, {
          location                                          :: #mlapi_location{}
         }).

-record(mlapi_geolocation, {
          ip                                                :: mlapi_ip_address(),
          country_id                                        :: mlapi_country_id(),
          country_name                                      :: binary(),
          state_id                                          :: mlapi_state_id(),
          state_name                                        :: binary(),
          city_id                                           :: mlapi_city_id(),
          city_name                                         :: binary()
         }).

-record(mlapi_country, {
          id                                                :: mlapi_country_id(),
          name                                              :: binary(),
          locale                                            :: mlapi_locale_id(),
          currency_id                                       :: mlapi_currency_id()
         }).

-record(mlapi_state, {
          id                                                :: mlapi_state_id(),
          name                                              :: binary()
         }).

-record(mlapi_city, {
          id                                                :: mlapi_city_id(),
          name                                              :: binary()
         }).

-record(mlapi_country_ext, {
          id                                                :: mlapi_country_id(),
          name                                              :: binary(),
          locale                                            :: mlapi_locale_id(),
          currency_id                                       :: mlapi_currency_id(),
          decimal_separator                                 :: integer(),
          thousands_separator                               :: integer(),
          time_zone                                         :: mlapi_timezone_id(),
          geo_information                                   :: #mlapi_geo_information{},
          states = []                                       :: [#mlapi_state{}]
         }).

-record(mlapi_state_ext, {
          id                                                :: mlapi_state_id(),
          name                                              :: binary(),
          country                                           :: #mlapi_country{},
          geo_information                                   :: #mlapi_geo_information{},
          cities = []                                       :: [#mlapi_city{}]
         }).

-record(mlapi_city_ext, {
          id                                                :: mlapi_city_id(),
          name                                              :: binary(),
          state                                             :: #mlapi_state{},
          country                                           :: #mlapi_country{},
          geo_information                                   :: #mlapi_geo_information{}
         }).

-record(mlapi_ratings, {
          positive,
          negative,
          neutral
         }).

-record(mlapi_period, {
          total = 0,
          completed = 0,
          canceled = 0,
          ratings                                           :: #mlapi_ratings{}
         }).

-record(mlapi_transactions, {
          period                                            :: mlapi_period_id(),
          total                                             :: integer(),
          completed                                         :: integer(),
          canceled                                          :: integer(),
          ratings                                           :: #mlapi_ratings{}
         }).

-record(mlapi_seller_reputation, {
          level_id                                          :: mlapi_seller_level_id(),
          power_seller_status                               :: mlapi_power_seller_status_id(),
          transactions                                      :: #mlapi_transactions{}
         }).

-record(mlapi_buyer_reputation, {
          transactions                                      :: #mlapi_transactions{}
         }).

-record(mlapi_status, {
          site_status                                       :: mlapi_site_status_id(),
          list                                              :: binary()
         }).

-record(mlapi_identification, {
          identification_type,
          identification_number
         }).

-record(mlapi_phone, {
          area_code,
          number,
          verified                                          :: boolean()
         }).

-record(mlapi_user, {
          id,
          nickname,
          registration_date                                 :: calendar:datetime(),
          first_name                                        :: binary(),
          last_name                                         :: binary(),
          country_id                                        :: mlapi_country_id(),
          email                                             :: mlapi_email_address(),
          identification                                    :: #mlapi_identification{},
          phone                                             :: #mlapi_phone{},
          user_type                                         :: mlapi_user_type_id(),
          logo,
          points = 0,
          site_id                                           :: mlapi_site_id(),
          permalink                                         :: mlapi_url(),
          seller_experience                                 :: mlapi_seller_experience_id(),
          seller_reputation                                 :: #mlapi_seller_reputation{},
          buyer_reputation                                  :: #mlapi_buyer_reputation{},
          status                                            :: #mlapi_status{}
         }).

-record(mlapi_user_status, {
          site_status
         }).


-record(mlapi_picture, {
          id                                                :: mlapi_picture_id(),
          url,
          secure_url,
          size,
          max_size,
          quality
         }).

-record(mlapi_description, {
          id                                                :: mlapi_description_id(),
          created
         }).

-record(mlapi_payment_type, {
          id                                                :: mlapi_payment_type_id(),
          name                                              :: binary()
         }).

-record(mlapi_card_issuer, {
          %% site_id added to be able to store the record in Mnesia
          site_id                                           :: mlapi_site_id(),
          id                                                :: mlapi_card_issuer_id(),
          name                                              :: binary()
         }).

-record(mlapi_payment_method, {
          %% site_id added to be able to store the record in Mnesia
          site_id                                           :: mlapi_site_id(),
          id                                                :: mlapi_payment_method_id(),
          name                                              :: binary(),
          payment_type_id                                   :: mlapi_payment_type_id(),
          thumbnail                                         :: mlapi_url(),
          secure_thumbnail                                  :: mlapi_url()
         }).

-record(mlapi_card_issuer_ext, {
          site_id                                           :: mlapi_site_id(),
          id                                                :: mlapi_card_issuer_id(),
          name                                              :: binary(),
          %% INCONSISTENT: the payment method here only returns: id, name
          payment_methods                                   :: [#mlapi_payment_method{}]
         }).

-record(mlapi_payer_costs, {
          installments                                      :: non_neg_integer(),
          installment_rate                                  :: float(),
          labels = [],
          min_allowed_amount                                :: float(),
          max_allowed_amount                                :: float(),
          minimum_charge                                    :: float()
         }).

-record(mlapi_exceptions_by_card_issuer, {
          card_issuer                                       :: #mlapi_card_issuer{},
          labels = []                                       :: [binary()],
          thumbnail                                         :: mlapi_url(),
          secure_thumbnail                                  :: mlapi_url(),
          payer_costs                                       :: [#mlapi_payer_costs{}],
          total_financial_cost                              :: float()
         }).

-record(mlapi_payment_method_ext, {
          site_id                                           :: mlapi_site_id(),
          id                                                :: mlapi_payment_method_id(),
          name                                              :: binary(),
          payment_type_id                                   :: mlapi_payment_type_id(),
          secure_thumbnail                                  :: mlapi_url(),
          thumbnail                                         :: mlapi_url(),
          labels = [],
          min_accreditation_days                            :: non_neg_integer(),
          max_accreditation_days                            :: non_neg_integer(),
          payer_costs                                       :: [#mlapi_payer_costs{}],
          avs_enabled                                       :: boolean(),
          exceptions_by_card_issuer                         :: [#mlapi_card_issuer{}]
         }).

-record(mlapi_shipping_costs, {
          description,
          cost                                              :: float(),
          time,
          shipping_rule_id
         }).

-record(mlapi_shipping, {
          profile_id,
          local_pick_up                                     :: boolean(),
          free_shipping                                     :: boolean(),
          costs                                             :: #mlapi_shipping_costs{}
         }).

-record(mlapi_seller_address, {
          id                                                :: mlapi_address_id(),
          comment                                           :: binary(),
          address_line                                      :: binary(),
          zip_code                                          :: binary(),
          city                                              :: #mlapi_city{},
          state                                             :: #mlapi_state{},
          country                                           :: #mlapi_country{},
          latitude = 0.0                                    :: float(),
          longitude = 0.0                                   :: float()
         }).

-record(mlapi_attribute, {
          id                                                :: mlapi_attribute_id(),
          name                                              :: binary(),
          value_id,
          value_name,
          attribute_group_id,
          attribute_group_name
         }).

-record(mlapi_attribute_combination, {
          id,
          name,
          value_id,
          value_name
         }).

-record(mlapi_varying_attribute, {
          attribute_id,
          attribute_name,
          values = []
         }).

-record(mlapi_variations, {
          id,
          attribute_combinations = []                       :: [#mlapi_attribute_combination{}],
          price = 0.0                                       :: float(),
          available_quantity                                :: integer(),
          picture_id                                        :: mlapi_picture_id(),
          seller_custom_field                               :: binary()
         }).

-record(mlapi_item, {
          id                                                :: mlapi_item_id(),
          site_id                                           :: mlapi_site_id(),
          title                                             :: binary(),
          subtitle                                          :: binary(),
          seller_id                                         :: mlapi_user_id(),
          category_id                                       :: mlapi_category_id(),
          price = 0.0                                       :: float(),
          base_price = 0.0                                  :: float(),
          currency_id                                       :: mlapi_currency_id(),
          initial_quantity = 0                              :: integer(),
          available_quantity = 0                            :: integer(),
          sold_quantity = 0                                 :: integer(),
          buying_mode                                       :: mlapi_buying_mode_id(),
          listing_type_id                                   :: mlapi_listing_type_id(),
          start_time                                        :: calendar:datetime(),
          stop_time                                         :: calendar:datetime(),
          condition                                         :: mlapi_item_condition_id(),
          permalink                                         :: mlapi_url(),
          thumbnail                                         :: mlapi_url(),
          pictures = []                                     :: [#mlapi_picture{}],
          video_id,
          descriptions = []                                 :: [#mlapi_description{}],
          accepts_mercadopago                               :: boolean(),
          non_mercado_pago_payment_methods = []             :: [#mlapi_payment_method{}],
          shipping                                          :: #mlapi_shipping{},
          seller_address                                    :: #mlapi_seller_address{},
          seller_contact                                    :: binary(),
          location,
          geolocation                                       :: #mlapi_location{},
          coverage_areas = [],
          attributes = []                                   :: [#mlapi_attribute{}],
          varying_attributes = []                           :: [#mlapi_varying_attribute{}],
          variations = []                                   :: [#mlapi_variations{}],
          status                                            :: mlapi_item_status_id(),
          sub_status = [],
          warranty,
          catalog_product_id,
          seller_custom_field                               :: binary(),
          parent_item_id                                    :: mlapi_item_id(),
          date_created,
          last_updated
         }).

-record(mlapi_seller, {
          id                                                :: mlapi_user_id(),
          seller_reputation,
          power_seller_status                               :: mlapi_power_seller_status_id(),
          real_estate_agency                                :: boolean(),
          car_dealer                                        :: boolean()
         }).

-record(mlapi_installment, {
          quantity = 0                                      :: integer(),
          amount = 0.0                                      :: float(),
          currency_id                                       :: mlapi_currency_id()
         }).

-record(mlapi_search_address, {
          state_id                                          :: mlapi_state_id(),
          state_name                                        :: binary(),
          city_id                                           :: mlapi_city_id(),
          city_name                                        :: binary()
         }).

-record(mlapi_search_item, {
          id                                                :: mlapi_item_id(),
          site_id                                           :: mlapi_site_id(),
          title                                             :: binary(),
          subtitle                                          :: binary(),
          seller                                            :: #mlapi_seller{},
          price = 0.0                                       :: float(),
          currency_id                                       :: mlapi_currency_id(),
          sold_quantity = 0                                 :: integer(),
          buying_mode                                       :: mlapi_buying_mode_id(),
          listing_type_id                                   :: mlapi_listing_type_id(),
          stop_time                                         :: calendar:datetime(),
          condition                                         :: mlapi_item_condition_id(),
          permalink                                         :: mlapi_url(),
          thumbnail                                         :: mlapi_url(),
          accepts_mercadopago                               :: boolean(),
          installments                                      :: #mlapi_installment{},
          address                                           :: #mlapi_search_address{},
          seller_address                                    :: #mlapi_seller_address{},
          attributes = []                                   :: [#mlapi_attribute{}]
         }).

-record(mlapi_paging, {
        total = 0                                           :: integer(),
        offset = 0                                          :: integer(),
        limit = 0                                           :: integer()
       }).

-record(mlapi_sort, {
        id,
        name
       }).

-record(mlapi_filter_value, {
          id,
          name,
          results
         }).

-record(mlapi_filter, {
          id,
          name,
          type,
          values                                            :: #mlapi_filter_value{}
         }).

-record(mlapi_search_result, {
          site_id                                          :: mlapi_site_id(),
          seller                                           :: #mlapi_seller{},
          'query'                                          :: binary(),
          results                                          :: [#mlapi_item{}],
          matching_catalog_products,
          paging                                           :: #mlapi_paging{},
          sort                                             :: #mlapi_sort{},
          available_sorts                                  :: #mlapi_sort{},
          filters,
          available_filters
         }).

-record(mlapi_trend, {
          keyword                                          :: binary(),
          url                                              :: mlapi_url()
         }).

-endif.
