MercadoLibre API Client for Erlang
==================================

[MercadoLibre][http://www.mercadolibre.com/] is the biggest e-commerce site in
Latin America (similar to eBay) and it has recently released a REST API to
retrieve information from the published items, the users and most of the actions
that can be performed in the site. The API returns its results encoded in
[JSON][http://www.json.org/] and has a public interface that is freely
accessible and a private interface that can only be accessed by a MercadoLibre
user using [OAuth][http://oauth.net/] authentication in an application.

This implementation only addresses the public side of the API so far, but it
will grow to support the private interface too. There are two main modules:
1)``mlapi``: which provides uncached access to the API; and 2) ``mlapi_cache``:
which caches the results returned by the API. Bear in mind that the current
implementation still does not clean up stale cache entries once they've expired
unless they are being overwritten.

Requirements
============
You will need a fairly recent version of [Erlang][http://www.erlang.org/] and
[rebar][https://github.com/basho/rebar] installed in your path. So far it has
only been tested with Erlang R14B03 on Ubuntu Linux 11.04 (Natty) but will
probably work with previous releases of R14 on other platforms too.

Installation
============
Clone the [mlapi][https://github.com/jcomellas/mlapi] repository and issue the
following commands:

  git clone https://github.com/jcomellas/mlapi.git
  cd mlapi
  make deps
  make

That will download all the required Erlang dependencies and compile the project.
After that you can start using the modules in it.

Usage
=====
You can easily test the modules within the Erlang shell. To enter the shell with
the required paths already set run:

  make console

Once you're in the Erlang shell you need to start the ``mlapi`` application. You
can start it and its dependencies by doing:

  mlapi:start().

Now we're ready to rock. Keep in mind the following type specifications:

  -type error()             :: {error, Reason :: atom() | {atom(), any()}}.
  -type ejson_key()         :: binary().
  -type ejson_value()       :: binary() | boolean() | integer() | float() | 'null'.
  -type ejson()             :: {[{ejson_key(), ejson_value() | ejson()}]}.
  -type proplist()          :: [proplists:property()].
  -type format()            :: 'binary' | 'ejson' | 'proplist' | 'orddict' | 'record'.
  -type option()            :: {format, format()} | {record, RecordName :: atom()} | 'refresh'.
  -type response()          :: binary() | ejson() | proplist() | orddict:orddict() | tuple() | error().

All of the available functions that retrieve information from [MLAPI][http://www.mercadolibre.io/]
are very similar and follow a syntax like the following one:

  -spec mlapi:get_user(mlapi_user_id(), [mlapi:option()]) -> mlapi:response().

This is also a short version like:

  -spec mlapi:get_user(mlapi_user_id()) -> mlapi:response().

All the functions can receive options in the last argument. The most important
one would be the one to specify the format of the result. It follows the syntax:

  {format, Format :: mlapi:format()}

where ``Format`` can be one of:

<table>
  <tr><td>ejson</td><td>returns the JSON document as decoded by the [ejson][https://github.com/benoitc/ejson] Erlang library</td></tr>
  <tr><td>proplist</td><td>returns the parsed JSON document as a [property list][http://www.erlang.org/doc/man/proplists.html]</td></tr>
  <tr><td>orddict</td><td>returns the parsed JSON document as an [orddict][http://www.erlang.org/doc/man/orddict.html]</td></tr>
  <tr><td>record</td><td>returns the parsed JSON document as the corresponding record as defined in the ``mlapi.hrl`` header</td></tr>
  <tr><td>binary</td><td>returns the unparsed binary with the JSON document</td></tr>
</table>

For example, if we wanted to format the result as a proplist we'd do:

  mlapi:get_sites([{format, proplist}]).

And we'd receive:

  [[{id,<<"MLA">>},{name,<<"Argentina">>}],
   [{id,<<"MLB">>},{name,<<"Brasil">>}],
   [{id,<<"MCO">>},{name,<<"Colombia">>}],
   [{id,<<"MCR">>},{name,<<"Costa Rica">>}],
   [{id,<<"MEC">>},{name,<<"Ecuador">>}],
   [{id,<<"MLC">>},{name,<<"Chile">>}],
   [{id,<<"MLM">>},{name,<<"Mexico">>}],
   [{id,<<"MLU">>},{name,<<"Uruguay">>}],
   [{id,<<"MLV">>},{name,<<"Venezuela">>}],
   [{id,<<"MPA">>},{name,<<"Panamá">>}],
   [{id,<<"MPE">>},{name,<<"Perú">>}],
   [{id,<<"MPT">>},{name,<<"Portugal">>}],
   [{id,<<"MRD">>},{name,<<"Dominicana">>}]]

You can check the exported functions in ``src/mlapi.erl`` to see the complete interface.

Cached Interface
================
There is a variant of the ``mlapi`` module called ``mlapi_cache`` that caches
the results it receives in Mnesia. The time-to-live of each type of result can
be specified in the ``mlapi_metatable`` Mnesia table (see ``src/mlapi_cache.erl``
for its definition). The interface is the same as the one provided by the
``mlapi`` module with the caveat that the ``mlapi`` module admits both binaries
and strings for the identifiers and ``mlapi_cache`` only allows binaries.

Accessing the Documents
=======================
The resulting documents can be accessed very easily with the use of normal
Erlang tools. In particular, I'd recommend Bob Ippolito's [kvc][https://github.com/etrepum/kvc]
for the proplist format and Anton Lavrik's [erlson][https://github.com/alavrik/erlson.git]
for the orddict format.
