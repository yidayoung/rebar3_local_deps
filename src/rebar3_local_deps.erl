-module(rebar3_local_deps).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
    State1 = rebar_state:set_resources(State0, []),
    State2 = rebar_state:add_resource(State1, {git, rebar3_local_git_resource}),
    State3 = rebar_state:add_resource(State2, {git_subdir, rebar3_local_git_resource}),
    State4 = rebar_state:add_resource(State3, {pkg, rebar3_local_git_resource}),
    State5 = rebar_state:add_resource(State4, {hg, rebar3_local_git_resource}),
    {ok, State5}.
