-ifndef(MLAPI_HRL).
-define(MLAPI_HRL, "mlapi.hrl").

-record(mlapi_site, {
          id,
          name,
          country_id,
          sale_fees_mode                                    :: <<"not_free">>,
          mercadopago_version                               :: 3,
          default_currency_id,
          currencies = []                                   :: [#mlapi_currency{}],
          immediate_payment                                 :: <<"required">> | <<"optional">>,
          payment_method_ids = [],
          categories = []                                   [#mlapi_category{}],
         }).

-record(mlapi_country, {
          id,
          iso_code,
          name,
          locale,
          currency_id,
          status
         }).

-record(mlapi_state, {
          id,
          iso_code,
          name,
          status
         }).

-record(mlapi_city, {
          id,
          iso_code,
          name,
          status
         }).

-record(mlapi_currency, {
          id,
          description,
          symbol,
          decimal_places = 2
         }).

-record(mlapi_category, {
          id,
          name,
          permalink,
          total_items_in_this_category,
          path_from_root = [],
          children_categories = []
          settings                                          :: #mlapi_settings{},
         }).

-record(mlapi_settings, {
          adult_content                                     :: boolean(),
          buying_allowed                                    :: boolean(),
          buying_modes = []                                 :: [<<"buy_it_now">> | <<"auction">>],
          coverage_areas                                    :: <<"not_allowed">>,
          immediate_payment                                 :: <<"required">> | <<"optional">>,
          item_conditions = []                              :: [<<"not_specified">> | <<"new">> | <<"used">>],
          items_reviews_allowed                             :: boolean(),
          listing_allowed                                   :: boolean(),
          max_stock_per_item                                :: integer(),
          mirror_category,
          price                                             :: <<"required">> | <<"optional">>,
          shipping_profile                                  :: <<"required">> | <<"optional">>,
          show_contact_information                          :: boolean(),
          simple_shipping                                   :: <<"required">> | <<"optional">>,
          stock                                             :: <<"required">> | <<"optional">>,
          tags = []
         }).


-record(mlapi_user, {
          id,
          nickname,
          registration_date,
          country_id,
          user_type,
          logo,
          points = 0,
          site_id,
          permalink,
          seller_reputation                                 :: #mlapi_seller_reputation{},
          status
         }).

-record(mlapi_seller_reputation, {
          level_id,
          power_seller_status,
          transactions,
          period                                            :: #mlapi_period{}
         }).

-record(mlapi_period, {
          total = 0,
          completed = 0,
          canceled = 0,
          ratings                                           :: #mlapi_ratings{}
         }).

-record(mlapi_ratings, {
          positive,
          negative,
          neutral
         }).

-record(mlapi_user_status, {
          site_status
         }).


-record(mlapi_item, {
          id,
          site_id,
          title,
          subtitle,
          seller_id,
          category_id,
          price = 0.0,
          currency_id,
          initial_quantity = 0,
          available_quantity = 0,
          sold_quantity,
          buying_mode                                       :: <<"auction">> | <<"buy_it_now">>,
          listing_type_id                                   :: <<"gold">> | <<"silver">> | <<"bronze">>,
          start_time,
          stop_time,
          condition                                         :: <<"new">> | <<"used">> | <<"unspecified">>,
          permalink,
          thumbnail,
          pictures = []                                     :: [#mlapi_picture{}],
          video_id,
          descriptions = []                                 :: [#mlapi_description{}],
          accepts_mercadopago                               :: boolean() | undefined,
          non_mercadopago_payment_methods = []              :: [#mlapi_payment_method{}],
          shipping                                          :: #mlapi_shipping{},
          address                                           :: #mlapi_address{},
          coverage_areas = [],
          attributes = []                                   :: [#mlapi_attribute{}],
          varying_attributes = []                           :: [#mlapi_varying_attribute{}],
          variations = []                                   :: [#mlapi_variations{}],
          status                                            :: <<"not_yet_active">> | <<"paused">> | <<"active">> |
                                                               <<"closed">> | <<"deleted">> | <<"invisible">> |
                                                               <<"under_review">> | <<"suspended_by_user">>,
          sub_status = [],
          warranty,
          catalog_product_id,
          parent_item_id,
          date_created,
          last_updated
         }).


-record(mlapi_picture, {
          id,
          url,
          quality
         }).

-record(mlapi_description, {
          id,
          created
         }).

-record(mlapi_payment_method, {
          id,
          description,
          is_default                                        :: boolean(),
          type
         }).

-record(mlapi_shipping, {
          profile_id,
          local_pick_up                                     :: boolean(),
          free_shipping                                     :: boolean()
         }).

-record(mlapi_address, {
          id,
          country_id,
          country_name,
          state_id,
          state_name,
          city_id,
          city_name,
          neighborhood_id,
          neighborhood_name,
          zip_code,
          street,
          contact,
          area_code,
          phone1,
          phone2,
          webpage,
          latitude = 0.0,
          longitude = 0.0
         }).

-record(mlapi_attribute, {
          id,
          name,
          value_id,
          value_name,
          attribute_group_id,
          attribute_group_name
         }).

-record(mlapi_varying_attribute, {
          attribute_id,
          attribute_name,
          values = []
         }).

-record(mlapi_variations, {
          id,
          attribute_combinations = []                       :: [#mlapi_attribute_combination{}],
          price = 0.0,
          available_quantity,
          picture_id,
          seller_custom_field
         }).

-record(mlapi_attribute_combination, {
          id,
          name,
          value_id,
          value_name
         }).



-endif().
