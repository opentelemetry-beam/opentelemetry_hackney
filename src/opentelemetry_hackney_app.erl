%%%-------------------------------------------------------------------
%% @doc opentelemetry_hackney public API
%% @end
%%%-------------------------------------------------------------------

-module(opentelemetry_hackney_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    opentelemetry_hackney_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
