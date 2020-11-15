%%%-------------------------------------------------------------------
%% @doc opentelemetry_hackney public API
%% @end
%%%-------------------------------------------------------------------

-module(opentelemetry_hackney_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% register as a tracer
    %% this is the only reason we are a runnable application
    {ok, Vsn} = application:get_key(opentelemetry_hackney, vsn),
    _ = opentelemetry:register_tracer(opentelemetry_hackney, Vsn),

    opentelemetry_hackney_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
