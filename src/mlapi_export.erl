%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2011 Juan Jose Comellas
%%% @doc MercadoLibre API module to export results to files.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(mlapi_export).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-include("include/mlapi.hrl").

-export([search/3, my_orders/3]).

%% Limit imposed my MLAPI when returning paged results.
-define(DEFAULT_OFFSET, 0).
-define(DEFAULT_LIMIT, 50).
-define(DEFAULT_FORMAT, json).

-type position()                                :: first | next | last.
-type format()                                  :: json | csv.
-type page_number()                             :: non_neg_integer().
-type page_count()                              :: non_neg_integer().
-type option_name()                             :: atom().
-type option_value()                            :: term().
-type option()                                  :: option_name() | {option_name(), option_value()}.



-spec search(file:filename(), [mlapi_site_id()], [option()]) -> ok | {error, Reason :: term()}.
search(Filename, [SiteId], Options) ->
    Args = filter_options(Options, [nickname, seller_id, category, q]),
    FetchPage = fun (Offset, Limit) -> mlapi_cache:search(SiteId, [{offset, Offset}, {limit, Limit} | Args], [{format, ejson}]) end,
    FormatDoc = proplists:get_value(format_fun, Options, fun encode_ejson/2),
    fetch_paged_response(Filename, FetchPage, FormatDoc, Options).


-spec my_orders(file:filename(), [], [option()]) -> ok | {error, Reason :: term()}.
my_orders(Filename, [], Options) ->
    Args = filter_options(Options, [access_token, feedback_status, payment_status, seller, shipping_status, sort]),
    FetchPage = fun (Offset, Limit) -> mlapi_cache:my_orders([{offset, Offset}, {limit, Limit} | Args], [{format, ejson}]) end,
    FormatDoc = proplists:get_value(format_fun, Options, fun encode_ejson/2),
    fetch_paged_response(Filename, FetchPage, FormatDoc, Options).




-spec encode_ejson(position(), mlapi:ejson()) -> iolist().
encode_ejson(Position, Doc) ->
    EncodedDoc = ejson:encode(Doc),
    if
        Position =:= first orelse Position =:= next ->
            [EncodedDoc, <<",\n">>];
        Position =:= last ->
            [EncodedDoc, <<"\n">>]
    end.


-spec fetch_paged_response(file:filename(), FetchPage :: fun(), FormatDoc :: fun(), [option()]) -> ok | {error, Reason :: term()}.
fetch_paged_response(Filename, FetchPage, FormatDoc, Options) when is_function(FetchPage), is_function(FormatDoc) ->
    {ok, File} = file:open(Filename, [write, binary]),
    Format = proplists:get_value(format, Options, ?DEFAULT_FORMAT),
    Offset = proplists:get_value(offset, Options, ?DEFAULT_OFFSET),
    Limit = proplists:get_value(limit, Options, ?DEFAULT_LIMIT),
    ok = write_header(File, Format),
    {ok, Pager} = mlapi_pager:start_link([{fetch_page, FetchPage}, {offset, Offset}, {limit, Limit}]),
    Result = fetch_pages(File, Pager, FormatDoc),
    ok = write_footer(File, Format),
    file:close(File),
    Result.


-spec fetch_pages(file:io_device(), Pager :: pid(), FormatDoc :: fun()) -> ok | {error, Reason :: term()}.
fetch_pages(File, Pager, FormatDoc) ->
    case mlapi_pager:next(Pager) of
        {error, _Reason} = Error ->
            Error;
        {{PageNumber, PageCount} = Position, Page} ->
            case kvc:path(<<"results">>, Page) of
                [] ->
                    {error, {results_not_found, Page}};
                Results ->
                    ok = write_page(File, Position, FormatDoc, Results),
                    if
                        PageNumber < PageCount ->
                            fetch_pages(File, Pager, FormatDoc);
                        true ->
                            ok
                    end
            end
    end.


-spec write_header(file:io_device(), format()) -> ok.
write_header(File, json) ->
    ok = file:write(File, <<"[\n">>).


-spec write_footer(file:io_device(), format()) -> ok.
write_footer(File, json) ->
    ok = file:write(File, <<"]\n">>).


-spec write_page(file:io_device(), {page_number(), page_count()}, FormatDoc :: fun(), [mlapi:ejson()]) -> ok | {error, Reason :: term()}.
write_page(File, {1, 1}, FormatDoc, [_Head] = Results) ->
    write_last_page(File, FormatDoc, Results);
write_page(File, {1, 1}, FormatDoc, [Head | Tail]) ->
    ok = file:write(File, FormatDoc(first, Head)),
    write_last_page(File, FormatDoc, Tail);
write_page(File, {PageNumber, PageNumber}, FormatDoc, Results) ->
    write_last_page(File, FormatDoc, Results);
write_page(File, {_PageNumber, _PageCount}, FormatDoc, Results) ->
    write_next_page(File, FormatDoc, Results).


-spec write_next_page(file:io_device(), FormatDoc :: fun(), [mlapi:ejson()]) -> ok | {error, Reason :: term()}.
write_next_page(File, FormatDoc, [Head | Tail]) ->
    ok = file:write(File, FormatDoc(next, Head)),
    write_next_page(File, FormatDoc, Tail);
write_next_page(_File, _FormatDoc, []) ->
    ok.


-spec write_last_page(file:io_device(), FormatDoc :: fun(), [mlapi:ejson()]) -> ok | {error, Reason :: term()}.
write_last_page(File, FormatDoc, [Head]) ->
    ok = file:write(File, FormatDoc(last, Head));
write_last_page(File, FormatDoc, [Head | Tail]) ->
    ok = file:write(File, FormatDoc(next, Head)),
    write_last_page(File, FormatDoc, Tail).


-spec filter_options([tuple()], [atom()]) -> [tuple()].
filter_options(Options, ValidNames) ->
    [Option || {Name, _Value} = Option <- Options, lists:member(Name, ValidNames)].
