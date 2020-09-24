-module(rebar3_local_deps_prv).

%% API
-export([init/1, do/1]).

-define(PROVIDER, 'compile').
-define(DEPS, []).
-define(SHORT_DESC, "Replace all deps to my_deps git source").
-define(DESC, "Configure local_deps_url options in your rebar.config, e.g.\n"
"  {local_deps_url, \"http://10.0.11.240:9001/r/public/\"").


init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {hooks, {[rebar_prv_help], [rebar_prv_install_deps]}},
        {module, ?MODULE},            % The module implementation of the task
        {bare, false},                 % The task can be run by the user, always true
        {deps, ?DEPS},                % The list of dependencies
        {opts, []},                   % list of options understood by the plugin
        {example, "rebar3 protobuf compile"},
        {short_desc, ?SHORT_DESC},
        {desc, ?DESC}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    LocalDepUrl = rebar_state:get(State, local_deps_url, undefined),
    io:format("LocalDepUrl is ~p~n",[LocalDepUrl]),
    case LocalDepUrl of
        undefined ->
            {ok, State};
        _ ->
            do_(State, LocalDepUrl)
    end.

update_deps(ParsedDeps, LocalDepUrl) ->
    [begin
         Source = rebar_app_info:source(Dep),
         rebar_app_info:source(Dep, ensure_source(Source, LocalDepUrl))
     end
        || Dep <- ParsedDeps
    ].

do_(State, LocalDepUrl) ->
    Profiles = rebar_state:current_profiles(State),
    State2 = lists:foldl(fun(Profile, StateAcc) ->
        ParsedDeps = rebar_state:get(State, {parsed_deps, Profile}, []),
        UpdatedDeps = update_deps(ParsedDeps, LocalDepUrl),
        rebar_state:set(StateAcc, {parsed_deps, Profile}, UpdatedDeps)
                         end, State, Profiles),
    {ok, State2}.

ensure_source({pkg, PkgName, Vsn, _Hash}, LocalDepUrl) ->
    ensure_source({git, erlang:binary_to_list(<<PkgName/binary, ".git">>), {tag, Vsn}}, LocalDepUrl);
ensure_source({pkg, PkgName, Vsn, _OldHash, _Hash}, LocalDepUrl) ->
    ensure_source({git, erlang:binary_to_list(<<PkgName/binary, ".git">>), {tag, Vsn}}, LocalDepUrl);
ensure_source(Source, LocalDepUrl) when is_tuple(Source)->
    case element(1,Source) of
        git ->
            Url = element(2, Source),
            Url1 = ensure_my_dep_dir(Url, LocalDepUrl),
            setelement(2, Source, Url1);
        _ ->
            Source
    end;
ensure_source(Source, _LocalDepUrl) ->
    Source.

ensure_my_dep_dir(Url, LocalDepUrl) ->
    Basename = filename:basename(Url),
    lists:append(LocalDepUrl, Basename).
