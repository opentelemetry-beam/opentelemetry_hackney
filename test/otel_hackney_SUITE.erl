-module(otel_hackney_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").
-include_lib("elli/include/elli.hrl").

all() ->
    [{group, w3c}, {group, b3}].

groups() ->
    [{w3c, [shuffle], [default_span_name, custom_span_name]},
     {b3, [shuffle], [default_span_name, custom_span_name]}].


init_per_suite(Config) ->
    ok = application:load(opentelemetry),
    application:ensure_all_started(hackney),
    Config.

end_per_suite(_Config) ->
    application:stop(hackney),
    application:unload(opentelemetry),
    ok.

init_per_group(Propagator, Config) ->
    application:set_env(opentelemetry, processors, [{otel_batch_processor, #{scheduled_delay_ms => 1}}]),
    {ok, _} = application:ensure_all_started(opentelemetry),

    {BaggageHttpExtractor, BaggageHttpInjector} = otel_baggage:get_text_map_propagators(),
    {TraceHttpExtractor, TraceHttpInjector} = case Propagator of
                                                  w3c -> otel_tracer_default:w3c_propagators();
                                                  b3 -> otel_tracer_default:b3_propagators()
                                              end,
    opentelemetry:set_text_map_extractors([BaggageHttpExtractor,
                                           TraceHttpExtractor]),
    opentelemetry:set_text_map_injectors([BaggageHttpInjector,
                                          TraceHttpInjector]),

    [{propagator, Propagator} | Config].

end_per_group(_, _Config)->
    application:stop(opentelemetry),
    ok.

init_per_testcase(_, Config) ->
    elli:start_link([{name, {local, elli_test_server}},
                     {port, 3000},
                     {callback, elli_middleware},
                     {callback_args, [{mods, [{otel_elli_middleware, []},
                                              {?MODULE, []}]}]}]),

    {ok, _} = application:ensure_all_started(opentelemetry),
    otel_batch_processor:set_exporter(otel_exporter_pid, self()),
    Config.

end_per_testcase(_, _Config) ->
    elli:stop(elli_test_server),
    _ = application:stop(opentelemetry),
    ok.

custom_span_name(_Config)->
    ?with_span(<<"remote-parent">>, #{},
               fun(_) ->
                       {ok, 200, _, _} =
                           otel_hackney:request(get, "http://localhost:3000/hello/otel?a=b#fragment", [], <<>>, [{span_name, fun(_, _) -> <<"/hello/{who}">> end}])
               end),

    receive
        {span, #span{name=Name,
                     span_id=SpanId,
                     parent_span_id=undefined,
                     attributes=_Attributes,
                     events=_TimeEvents}} ->
            ?assertEqual(<<"remote-parent">>, Name),
            receive
                {span, #span{name=ChildName,
                             parent_span_id=SpanId,
                             attributes=Attributes}} ->
                    ?assertEqual(<<"/hello/{who}">>, ChildName),
                    ?assert(lists:member({<<"http.method">>, <<"GET">>}, Attributes))
            after
                5000 ->
                    ct:fail(timeout)
            end
    after
        5000 ->
            ct:fail(timeout)
    end,

    ok.

default_span_name(_Config) ->
    ?with_span(<<"remote-parent">>, #{},
               fun(_) ->
                       {ok, 200, _, _} =
                           otel_hackney:request("http://localhost:3000/hello/otel?a=b#fragment")
               end),

    receive
        {span, #span{name=Name,
                     span_id=SpanId,
                     parent_span_id=undefined}} ->
            ?assertEqual(<<"remote-parent">>, Name),
            receive
                {span, #span{name=ChildName,
                             parent_span_id=SpanId,
                             attributes=Attributes}} ->
                    ?assertEqual(<<"HTTP GET">>, ChildName),
                    ?assert(lists:member({<<"http.method">>, <<"GET">>}, Attributes))
            after
                5000 ->
                    ct:fail(timeout)
            end
    after
        5000 ->
            ct:fail(timeout)
    end,

    ok.

%%

handle(Req, Args) ->
    handle(Req#req.path, Req, Args).

handle([<<"hello">>, Who], _Req, _Args) ->
    {ok, [], <<"Hello ", Who/binary>>};
handle([<<"error">>], _Req, _Args) ->
    throw(all_hell).

handle_event(_Event, _Data, _Args) ->
    ok.
