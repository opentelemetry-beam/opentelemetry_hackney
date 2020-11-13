-module(otel_hackney).

-export([request/1,
         request/2,
         request/3,
         request/4,
         request/5,
         request/6]).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").

%% @doc make a request
-spec request(hackney:url()|binary()|list())
             -> {ok, integer(), list(), hackney:client_ref()}
              | {ok, integer(), list()}
              | {error, term()}.
request(URL) ->
    request(get, URL).

%% @doc make a request
-spec request(term(), hackney:url()|binary()|list())
             -> {ok, integer(), list(), hackney:client_ref()}
              | {ok, integer(), list()}
              | {error, term()}.
request(Method, URL) ->
    request(Method, URL, [], <<>>, []).

%% @doc make a request
-spec request(term(), hackney:url()|binary()|list(), list())
             -> {ok, integer(), list(), hackney:client_ref()}
              | {ok, integer(), list()}
              | {error, term()}.
request(Method, URL, Headers) ->
    request(Method, URL, Headers, <<>>, []).

%% @doc make a request
-spec request(term(), hackney:url()|binary()|list(), list(), term())
             -> {ok, integer(), list(), hackney:client_ref()}
              | {ok, integer(), list()}
              | {error, term()}.
request(Method, URL, Headers, Body) ->
    request(Method, URL, Headers, Body, []).

-spec request(term(), hackney:url() | binary() | list(), list(), term(), list())
             -> {ok, integer(), list(), hackney:client_ref()}
              | {ok, integer(), list(), binary()}
              | {ok, integer(), list()}
              | {ok, hackney:client_ref()}
              | {error, term()}.
request(Method, URL, Headers0, Body, Options) ->
    Tracer = opentelemetry:get_tracer(?MODULE),
    request(Tracer, Method, URL, Headers0, Body, Options).

-spec request(opentelemetry:tracer(), term(), hackney:url() | binary() | list(), list(), term(), list())
             -> {ok, integer(), list(), hackney:client_ref()}
              | {ok, integer(), list(), binary()}
              | {ok, integer(), list()}
              | {ok, hackney:client_ref()}
              | {error, term()}.
request(Tracer, Method, URL, Headers0, Body, Options) ->
    Ctx = otel_ctx:get_current(),
    BinMethod = hackney_bstr:to_upper(hackney_bstr:to_binary(Method)),

    SpanName = case proplists:get_value(span_name, Options) of
                   SpanNameFun when is_function(SpanNameFun, 2) ->
                       SpanNameFun(Method, URL);
                   SpanName0 when is_binary(SpanName0) ->
                       SpanName0;
                   undefined ->
                       <<"HTTP ", BinMethod/binary>>
               end,

    Attributes = [{<<"http.url">>, URL},
                  {<<"http.flavor">>, <<"1.1">>},
                  {<<"http.method">>, BinMethod}],
    {SpanCtx, _} = otel_tracer:start_span(Ctx, Tracer, SpanName, #{kind => ?SPAN_KIND_CLIENT,
                                                                   attributes => Attributes}),
    Ctx1 = otel_tracer:set_current_span(Ctx, SpanCtx),
    otel_ctx:attach(Ctx1),
    try
        Headers1 = otel_propagator:text_map_inject(Headers0),
        hackney:request(Method, URL, Headers1, Body, Options)
    after
        %% passing SpanCtx directly ensures that this `end_span' ends the span started
        %% in this function. If spans in `Fun()' were started and not finished properly
        %% they will be abandoned and it be up to the `otel_span_sweeper' to eventually remove them.
        _ = otel_span:end_span(SpanCtx),
        otel_ctx:attach(Ctx)
    end.

%%
