-module(rebar3_local_deps).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
    {ok, State1} = rebar3_local_deps_prv:init(State0),
    {ok, State1}.
