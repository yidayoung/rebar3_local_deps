-module(rebar3_local_deps).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    LocalDepUrl = rebar_state:get(State, local_deps_url, undefined),
    case LocalDepUrl of
        undefined ->
            {ok, State};
        _ ->
            State1 = rebar_state:add_resource(State, {git, rebar3_local_git_resource}),
            State2 = rebar_state:add_resource(State1, {git_subdir, rebar3_local_git_resource}),
            State3 = rebar_state:add_resource(State2, {pkg, rebar3_local_git_resource}),
            State4 = rebar_state:add_resource(State3, {hg, rebar3_local_git_resource}),
            {ok, State4}
    end.
