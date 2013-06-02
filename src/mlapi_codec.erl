%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011-2012 Juan Jose Comellas
%%% @doc MercadoLibre API.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(mlapi_codec).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([decode/2, encode/2]).

-include("include/mlapi.hrl").

-compile([{parse_transform, dynarec}]).

-type json_text()         :: jsx:json_text().
-type json_term()         :: jsx:json_term().
-type json_name()         :: binary().
-type json_value()        :: json_term().
-type record_name()       :: atom().
-type field_name()        :: atom().
-type format()            :: json_term | proplist | record | dict | orddict | json_text | raw.
-type date_format()       :: iso8601 | datetime | epoch_seconds | gregorian_seconds.
-type decode_option()     :: {record, RecordName :: atom()} | {output_format, format()} | {date_format, date_format()}.
-type encode_option()     :: {record, RecordName :: atom()} | {input_format, format()} | {date_format, date_format()}.
-type decoded_msg()       :: json_term() | proplists:proplist() | tuple() | dict() | orddict:orddict() | json_text().
-type encoded_msg()       :: json_text().

%% Default formats
-define(FORMAT, json_term).
-define(DATE_FORMAT, datetime).


-spec decode(encoded_msg(), [decode_option()]) -> decoded_msg().
decode(Msg, Options) when is_binary(Msg), is_list(Options) ->
    RecordName = proplists:get_value(record, Options),
    OutputFormat = proplists:get_value(output_format, Options, ?FORMAT),
    DateFormat = proplists:get_value(date_format, Options, ?DATE_FORMAT),
    if
        OutputFormat =:= json_text orelse OutputFormat =:= raw ->
            Msg;
        OutputFormat =:= json_term ->
            jsx:decode(Msg);
        true ->
            Decode = case OutputFormat of
                         proplist  -> fun decode_proplist/3;
                         record    -> fun decode_record/3;
                         dict      -> fun decode_dict/3;
                         orddict   -> fun decode_orddict/3
                     end,
            JsonTerm = jsx:decode(Msg),
            Decode(RecordName, DateFormat, JsonTerm)
    end.


-spec encode(decoded_msg(), [encode_option()]) -> encoded_msg().
encode(Msg, Options) when (is_tuple(Msg) orelse is_list(Msg)), is_list(Options) ->
    RecordName = proplists:get_value(record, Options),
    InputFormat = proplists:get_value(input_format, Options, ?FORMAT),
    DateFormat = proplists:get_value(date_format, Options, ?DATE_FORMAT),
    if
        InputFormat =:= json_text orelse InputFormat =:= raw ->
            Msg;
        InputFormat =:= json_term ->
            jsx:encode(Msg);
        true ->
            Encode = case InputFormat of
                         proplist  -> fun encode_proplist/3;
                         record    -> fun encode_record/3;
                         dict      -> fun encode_dict/3;
                         orddict   -> fun encode_orddict/3
                     end,
            jsx:encode(Encode(RecordName, DateFormat, Msg))
    end.


%% @doc Convert a parsed JSON document or a list of documents into one or more property lists.
-spec decode_proplist(record_name(), date_format(), json_term()) -> proplists:proplist() | [proplists:proplist()].
decode_proplist(RecordName, DateFormat, [Head | _] = JsonTerm) when not is_list(Head) ->
    ChildDecoder = fun decode_proplist/3,
    lists:reverse(
      lists:foldl(fun ({Name, Value}, Proplist) ->
                          [decode_field(RecordName, ChildDecoder, DateFormat, Name, Value) | Proplist]
                  end, [], JsonTerm));
decode_proplist(RecordName, DateFormat, List) when is_list(List) ->
    decode_proplist_list(RecordName, DateFormat, List).


%% @doc Convert a list of parsed JSON documents into a list of property lists.
decode_proplist_list(RecordName, DateFormat, List) ->
    %% io:format("Processing array ~s~n", [RecordName]),
    lists:reverse(
      lists:foldl(fun (JsonTerm, Acc) ->
                          [decode_proplist(RecordName, DateFormat, JsonTerm) | Acc]
                  end, [], List)).


%% @doc Convert a parsed JSON document or a list of documents into one or more known records.
-spec decode_record(record_name(), date_format(), json_term()) -> tuple() | [tuple()].
decode_record(RecordName, DateFormat, [Head | _] = JsonTerm) when not is_list(Head) ->
    ChildDecoder = fun decode_record/3,
    lists:foldl(fun ({Name, Value}, Record) ->
                        {FieldName, DecodedValue} = decode_field(RecordName, ChildDecoder, DateFormat, Name, Value),
                        set_value(FieldName, DecodedValue, Record)
                end, new_record(RecordName), JsonTerm);
decode_record(RecordName, DateFormat, List) when is_list(List) ->
    decode_record_list(RecordName, DateFormat, List).


%% @doc Convert a list of parsed JSON documents into a list of records.
decode_record_list(RecordName, DateFormat, List) ->
    %% io:format("Processing array ~s~n", [RecordName]),
    lists:reverse(
      lists:foldl(fun (JsonTerm, Acc) ->
                          [decode_record(RecordName, DateFormat, JsonTerm) | Acc]
                  end, [], List)).


%% @doc Convert a parsed JSON document or a list of documents into one or more dicts.
-spec decode_dict(record_name(), date_format(), json_term()) -> Dict :: dict() | [dict()].
decode_dict(RecordName, DateFormat, [Head | _] = JsonTerm) when not is_list(Head) ->
    ChildDecoder = fun decode_dict/3,
    lists:foldl(fun ({Name, Value}, Dict) ->
                        {FieldName, DecodedValue} = decode_field(RecordName, ChildDecoder, DateFormat, Name, Value),
                        dict:append(FieldName, DecodedValue, Dict)
                end, dict:new(), JsonTerm);
decode_dict(RecordName, DateFormat, List) when is_list(List) ->
    decode_dict_list(RecordName, DateFormat, List).


%% @doc Convert a list of parsed JSON documents into a list of dicts.
decode_dict_list(RecordName, DateFormat, List) ->
    %% io:format("Processing array ~s~n", [RecordName]),
    lists:reverse(
      lists:foldl(fun (JsonTerm, Acc) ->
                          [decode_dict(RecordName, DateFormat, JsonTerm) | Acc]
                  end, [], List)).


%% @doc Convert a parsed JSON document or a list of documents into one or more orddicts.
-spec decode_orddict(record_name(), date_format(), json_term()) -> Orddict :: orddict:orddict() | [orddict:orddict()].
decode_orddict(RecordName, DateFormat, [Head | _] = JsonTerm) when not is_list(Head) ->
    ChildDecoder = fun decode_orddict/3,
    lists:foldl(fun ({Name, Value}, Orddict) ->
                        {FieldName, DecodedValue} = decode_field(RecordName, ChildDecoder, DateFormat, Name, Value),
                        orddict:append(FieldName, DecodedValue, Orddict)
                end, orddict:new(), JsonTerm);
decode_orddict(RecordName, DateFormat, List) when is_list(List) ->
    decode_orddict_list(RecordName, DateFormat, List).


%% @doc Convert a list of parsed JSON documents into a list of orddicts.
decode_orddict_list(RecordName, DateFormat, List) ->
    %% io:format("Processing array ~s~n", [RecordName]),
    lists:reverse(
      lists:foldl(fun (JsonTerm, Acc) ->
                          [decode_orddict(RecordName, DateFormat, JsonTerm) | Acc]
                  end, [], List)).


%% @doc Decode a JSON term into a format suitable to be stored in a proplist/record/dict/orddict.
-spec decode_field(record_name(), ChildDecoder :: fun(), date_format(),
                   json_name(), Value :: term()) -> {field_name(), json_value()}.
decode_field(RecordName, ChildDecoder, DateFormat, Name, Value) ->
    FieldName = binary_to_existing_atom(Name, utf8),
    NewValue =
        case child_record_name(RecordName, FieldName) of
            undefined ->
                case is_datetime_field(RecordName, FieldName) of
                    true  -> decode_datetime(DateFormat, Value);
                    false -> Value
                end;
            ChildRecordName ->
                if
                    is_list(Value) ->
                        %% io:format("Field ~s (record ~s) is a child of ~s~n", [FieldName, ChildRecordName, RecordName]),
                        ChildDecoder(ChildRecordName, DateFormat, Value);
                    true ->
                        Value
                end
        end,
    {FieldName, NewValue}.



%% @doc Convert a property list with (potentially) nested proplists to a
%%      parsed JSON document or a list of documents.
-spec encode_proplist(record_name(), date_format(), Proplist :: proplists:proplist() | [proplists:proplist()]) -> json_term().
encode_proplist(RecordName, DateFormat, [Head | _] = Proplist) when is_tuple(Head) ->
    ChildEncoder = fun encode_proplist/3,
    lists:reverse(
      lists:foldl(fun ({FieldName, Value}, Acc) ->
                          case encode_field(RecordName, ChildEncoder, DateFormat, FieldName, Value) of
                              {_EncodedName, undefined} -> Acc;
                              Element                   -> [Element | Acc]
                          end
                  end, [], Proplist));
encode_proplist(RecordName, DateFormat, [Head | _] = List) when is_list(Head) ->
    encode_proplist_list(RecordName, DateFormat, List).


%% @doc Convert  a list of property lists into a list of parsed JSON documents.
encode_proplist_list(RecordName, DateFormat, List) ->
    %% io:format("Processing array ~s~n", [RecordName]),
    lists:reverse(
      lists:foldl(fun (Proplist, Acc) ->
                          [encode_proplist(RecordName, DateFormat, Proplist) | Acc]
                  end, [], List)).


%% @doc Convert a record with (potentially) nested records into a parsed JSON
%%      document or a list of documents.
-spec encode_record(record_name(), date_format(), Record :: tuple() | [tuple()]) -> json_term().
encode_record(RecordName, DateFormat, Record) when is_tuple(Record) ->
    ChildEncoder = fun encode_record/3,
    Fields = fields(RecordName),
    lists:reverse(
      lists:foldl(fun (FieldName, Acc) ->
                          case encode_field(RecordName, ChildEncoder, DateFormat, FieldName, get_value(FieldName, Record)) of
                              {_EncodedName, undefined} -> Acc;
                              Element                   -> [Element | Acc]
                          end
                  end, [], Fields));
encode_record(RecordName, DateFormat, List) when is_list(List) ->
    encode_record_list(RecordName, DateFormat, List).


%% @doc Convert a list of records into a list of parsed JSON documents.
encode_record_list(RecordName, DateFormat, List) ->
    %% io:format("Processing array ~s~n", [RecordName]),
    lists:reverse(
      lists:foldl(fun (Record, Acc) ->
                          [encode_record(RecordName, DateFormat, Record) | Acc]
                  end, [], List)).


%% @doc Convert a dict or list of dicts into a parsed JSON document or a list
%%      of documents.
-spec encode_dict(record_name(), date_format(), Dict :: dict() | [dict()]) -> json_term().
encode_dict(RecordName, DateFormat, Dict) when not is_list(Dict) ->
    ChildEncoder = fun encode_dict/3,
    lists:reverse(
      dict:fold(fun (FieldName, [Value], Acc) ->
                        case encode_field(RecordName, ChildEncoder, DateFormat, FieldName, Value) of
                            {_EncodedName, undefined} -> Acc;
                            Element                   -> [Element | Acc]
                        end
                end, [], Dict));
encode_dict(RecordName, DateFormat, List) when is_list(List) ->
    encode_dict_list(RecordName, DateFormat, List).


%% @doc Convert a list of dicts into a list of parsed JSON documents.
encode_dict_list(RecordName, DateFormat, List) ->
    %% io:format("Processing array ~s~n", [RecordName]),
    lists:reverse(
      lists:foldl(fun (Dict, Acc) ->
                          [encode_dict(RecordName, DateFormat, Dict) | Acc]
                  end, [], List)).


%% @doc Convert an orddict or list of orddicts into a parsed JSON document or
%%      a list of documents.
-spec encode_orddict(record_name(), date_format(), Orddict :: orddict:orddict() | [orddict:orddict()]) -> json_term().
encode_orddict(RecordName, DateFormat, [Head | _] = Orddict) when not is_list(Head) ->
    ChildEncoder = fun encode_orddict/3,
    lists:reverse(
      orddict:fold(fun (FieldName, [Value], Acc) ->
                           case encode_field(RecordName, ChildEncoder, DateFormat, FieldName, Value) of
                               {_EncodedName, undefined} -> Acc;
                               Element                   -> [Element | Acc]
                           end
                end, [], Orddict));
encode_orddict(RecordName, DateFormat, List) ->
    encode_orddict_list(RecordName, DateFormat, List).


%% @doc Convert a list of orddicts into a list of parsed JSON documents.
encode_orddict_list(RecordName, DateFormat, List) ->
    %% io:format("Processing array ~s~n", [RecordName]),
    lists:reverse(
      lists:foldl(fun (Orddict, Acc) ->
                          [encode_orddict(RecordName, DateFormat, Orddict) | Acc]
                  end, [], List)).


%% @doc Encode a field as a tuple suitable to be stored in a JSON term.
-spec encode_field(record_name(), ChildEncoder :: fun(), date_format(),
                   field_name(), Value :: term()) -> {json_name(), json_value()}.
encode_field(RecordName, ChildEncoder, DateFormat, FieldName, Value) ->
    NewValue =
        case child_record_name(RecordName, FieldName) of
            undefined ->
                case is_datetime_field(RecordName, FieldName) of
                    true ->  encode_datetime(DateFormat, Value);
                    false -> Value
                end;
            ChildRecordName ->
                if
                    is_tuple(Value) orelse is_list(Value) ->
                        %% io:format("Field ~s (record ~s) is a child of ~s~n", [FieldName, ChildRecordName, RecordName]),
                        ChildEncoder(ChildRecordName, DateFormat, Value);
                    true ->
                        Value
                end
        end,
    {encode_field_name(FieldName), NewValue}.


%% @doc Return the record name for those JSON fields that can be converted to a known child record.
-spec child_record_name(ParentRecordName :: atom(), FieldName :: atom()) -> ChildRecordName :: atom() | undefined.
child_record_name(mlapi_address, city)                                 -> mlapi_city;
child_record_name(mlapi_address, country)                              -> mlapi_country;
child_record_name(mlapi_address, state)                                -> mlapi_state;
child_record_name(mlapi_buyer_reputation, transactions)                -> mlapi_buyer_transactions;
child_record_name(mlapi_buyer_transactions, canceled)                  -> mlapi_buyer_transaction_count;
child_record_name(mlapi_buyer_transactions, unrated)                   -> mlapi_buyer_transaction_count;
child_record_name(mlapi_buyer_transactions, not_yet_rated)             -> mlapi_buyer_transaction_count;
child_record_name(mlapi_catalog_product, pictures)                     -> mlapi_catalog_product_picture;
child_record_name(mlapi_catalog_product, specification)                -> mlapi_catalog_product_specification;
child_record_name(mlapi_catalog_product, searchable_attributes)        -> mlapi_attribute;
child_record_name(mlapi_catalog_product, user_reviews)                 -> mlapi_user_review;
child_record_name(mlapi_catalog_product_search, paging)                -> mlapi_paging;
child_record_name(mlapi_catalog_product_search, results)               -> mlapi_catalog_product_search_result;
child_record_name(mlapi_category, children_categories)                 -> mlapi_child_category;
child_record_name(mlapi_category, path_from_root)                      -> mlapi_category_path;
child_record_name(mlapi_category, settings)                            -> mlapi_settings;
child_record_name(mlapi_credit_level, exception_by_category)           -> mlapi_credit_exception_by_category;
child_record_name(mlapi_country_ext, states)                           -> mlapi_state;
child_record_name(mlapi_domain, attributes)                            -> mlapi_domain_attribute;
child_record_name(mlapi_exceptions_by_card_issuer, card_issuer)        -> mlapi_card_issuer;
child_record_name(mlapi_exceptions_by_card_issuer, payer_costs)        -> mlapi_payer_costs;
child_record_name(mlapi_feedback, sent)                                -> mlapi_feedback_issued;
child_record_name(mlapi_feedback, received)                            -> mlapi_feedback_issued;
child_record_name(mlapi_filter, values)                                -> mlapi_filter_value;
child_record_name(mlapi_filter_value, path_from_root)                  -> mlapi_category_path;
child_record_name(mlapi_geo_information, location)                     -> mlapi_location;
child_record_name(mlapi_item, attributes)                              -> mlapi_attribute;
child_record_name(mlapi_item, city)                                    -> mlapi_city;
child_record_name(mlapi_item, country)                                 -> mlapi_country;
child_record_name(mlapi_item, descriptions)                            -> mlapi_description;
child_record_name(mlapi_item, geolocation)                             -> mlapi_location;
child_record_name(mlapi_item, non_mercado_pago_payment_methods)        -> mlapi_non_mercadopago_payment_method;
child_record_name(mlapi_item, pictures)                                -> mlapi_item_picture;
child_record_name(mlapi_item, seller_address)                          -> mlapi_address;
child_record_name(mlapi_item, shipping)                                -> mlapi_shipping;
child_record_name(mlapi_item, state)                                   -> mlapi_state;
child_record_name(mlapi_item, variations)                              -> mlapi_item_variation;
child_record_name(mlapi_item, varying_attributes)                      -> mlapi_varying_attribute;
child_record_name(mlapi_order, buyer)                                  -> mlapi_buyer;
child_record_name(mlapi_order, feedback)                               -> mlapi_feedback;
child_record_name(mlapi_order, order_items)                            -> mlapi_order_item;
child_record_name(mlapi_order, payments)                               -> mlapi_payment;
child_record_name(mlapi_order, seller)                                 -> mlapi_order_seller;
child_record_name(mlapi_order, shipping)                               -> mlapi_order_shipping;
child_record_name(mlapi_order_item, item)                              -> mlapi_order_item_info;
child_record_name(mlapi_order_search, available_filters)               -> mlapi_filter;
child_record_name(mlapi_order_search, available_sorts)                 -> mlapi_sort;
child_record_name(mlapi_order_search, filters)                         -> mlapi_filter;
child_record_name(mlapi_order_search, paging)                          -> mlapi_paging;
child_record_name(mlapi_order_search, results)                         -> mlapi_order;
child_record_name(mlapi_order_search, sort)                            -> mlapi_sort;
child_record_name(mlapi_order_seller, phone)                           -> mlapi_phone;
child_record_name(mlapi_order_shipping, receiver_address)              -> mlapi_address;
child_record_name(mlapi_picture, variations)                           -> mlapi_picture_variation;
child_record_name(mlapi_payment_method_ext, card_configuration)        -> mlapi_card_configuration;
child_record_name(mlapi_payment_method_ext, exceptions_by_card_issuer) -> mlapi_exceptions_by_card_issuer;
child_record_name(mlapi_payment_method_ext, payer_costs)               -> mlapi_payer_costs;
child_record_name(mlapi_question, answer)                              -> mlapi_answer;
child_record_name(mlapi_question_result, questions)                    -> mlapi_question;
child_record_name(mlapi_sale, buyer)                                   -> mlapi_buyer;
child_record_name(mlapi_sale, order_items)                             -> mlapi_sale_item;
child_record_name(mlapi_sale, payment)                                 -> mlapi_payment;
child_record_name(mlapi_sale, feedback)                                -> mlapi_feedback;
child_record_name(mlapi_sale, shipping)                                -> mlapi_sale_shipping;
child_record_name(mlapi_sale_shipping, receiver_address)               -> mlapi_address;
child_record_name(mlapi_search_item, address)                          -> mlapi_search_address;
child_record_name(mlapi_search_item, attributes)                       -> mlapi_attribute;
child_record_name(mlapi_search_item, seller)                           -> mlapi_seller;
child_record_name(mlapi_search_item, installments)                     -> mlapi_installment;
child_record_name(mlapi_search_result, filters)                        -> mlapi_filter;
child_record_name(mlapi_search_result, available_filters)              -> mlapi_filter;
child_record_name(mlapi_search_result, paging)                         -> mlapi_paging;
child_record_name(mlapi_search_result, results)                        -> mlapi_search_item;
child_record_name(mlapi_search_result, seller)                         -> mlapi_seller;
child_record_name(mlapi_search_result, sort)                           -> mlapi_sort;
child_record_name(mlapi_search_result, available_sorts)                -> mlapi_sort;
child_record_name(mlapi_seller_reputation, transactions)               -> mlapi_seller_transactions;
child_record_name(mlapi_seller_transactions, ratings)                  -> mlapi_ratings;
child_record_name(mlapi_shipping, costs)                               -> mlapi_shipping_costs;
child_record_name(mlapi_site_ext, categories)                          -> mlapi_category_path;
child_record_name(mlapi_site_ext, currencies)                          -> mlapi_currency;
child_record_name(mlapi_state_ext, cities)                             -> mlapi_city;
child_record_name(mlapi_user, identification)                          -> mlapi_identification;
child_record_name(mlapi_user, buyer_reputation)                        -> mlapi_buyer_reputation;
child_record_name(mlapi_user, phone)                                   -> mlapi_phone;
child_record_name(mlapi_user, alternative_phone)                       -> mlapi_phone;
child_record_name(mlapi_user, seller_reputation)                       -> mlapi_seller_reputation;
child_record_name(mlapi_user, status)                                  -> mlapi_user_status;
child_record_name(mlapi_user, company)                                 -> mlapi_company;
child_record_name(mlapi_user, credit)                                  -> mlapi_user_credit;
child_record_name(mlapi_user_action_status, immediate_payment)         -> mlapi_immediate_payment;
child_record_name(mlapi_user_status, list)                             -> mlapi_user_action_status;
child_record_name(mlapi_user_status, buy)                              -> mlapi_user_action_status;
child_record_name(mlapi_user_status, sell)                             -> mlapi_user_action_status;
child_record_name(mlapi_user_status, billing)                          -> mlapi_user_action_status;
child_record_name(mlapi_user_review, stars_count)                      -> mlapi_stars_count;
child_record_name(_RecordName, geo_information)                        -> mlapi_geo_information;
child_record_name(_RecordName, _FieldName)                             -> undefined.


%% @doc Check whether a field of a record should be converted to a datetime.
-spec is_datetime_field(RecordName :: atom(), FieldName :: atom()) -> boolean().
is_datetime_field(mlapi_feedback_issued, date_created) -> true;
is_datetime_field(mlapi_payment, date_created)         -> true;
is_datetime_field(mlapi_sale, date_created)            -> true;
is_datetime_field(mlapi_sale_shipping, date_created)   -> true;
is_datetime_field(mlapi_shipping_costs, time)          -> true;
is_datetime_field(mlapi_item, date_created)            -> true;
is_datetime_field(mlapi_item, last_updated)            -> true;
is_datetime_field(mlapi_item, start_time)              -> true;
is_datetime_field(mlapi_item, stop_time)               -> true;
is_datetime_field(mlapi_order, date_closed)            -> true;
is_datetime_field(mlapi_order, date_created)           -> true;
is_datetime_field(mlapi_order_shipping, date_created)  -> true;
is_datetime_field(mlapi_search_item, stop_time)        -> true;
is_datetime_field(mlapi_user, registration_date)       -> true;
is_datetime_field(_RecordName, _FieldName)             -> false.


%% @doc Convert a datetime in the ISO-8601 format to the format specified in the
%%      first argument.
decode_datetime(iso8601, Value) ->           Value;
decode_datetime(datetime, Value) ->          mlapi_time:iso8601_to_datetime(Value);
decode_datetime(epoch_seconds, Value) ->     mlapi_time:iso8601_to_epoch(Value);
decode_datetime(gregorian_seconds, Value) -> mlapi_time:iso8601_to_gregorian_seconds(Value).


%% @doc Convert a datetime in the format specified in the first argument to the
%%      ISO-8601 format.
encode_datetime(iso8601, Value) ->           Value;
encode_datetime(datetime, Value) ->          mlapi_time:datetime_to_iso8601(Value);
encode_datetime(epoch_seconds, Value) ->     mlapi_time:epoch_to_iso8601(Value);
encode_datetime(gregorian_seconds, Value) -> mlapi_time:gregorian_seconds_to_iso8601(Value).


encode_field_name(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).
