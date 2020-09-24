-module(rebar3_local_git_resource).

%% API
-export([init/2, lock/2, download/4, needs_update/2, make_vsn/2]).


init(Type, State) ->
    {ok, Vsn} = application:get_key(rebar, vsn),
    BaseConfig = #{http_adapter => r3_hex_http_httpc,
        http_user_agent_fragment =>
        <<"(rebar3/", (list_to_binary(Vsn))/binary, ") (httpc)">>,
        http_adapter_config => #{profile => rebar}},
    Repos = rebar_hex_repos:from_state(BaseConfig, State),
    LocalGitUrl = rebar_state:get(State, local_deps_url, undefined),
    Resource = rebar_resource_v2:new(Type, ?MODULE, #{repos => Repos,
        base_config => BaseConfig, local_git_url => LocalGitUrl}),
    {ok, Resource}.

lock(AppInfo, #{local_git_url:=LocalGitUrl}) ->
    AppInfo2 = update_app_info(AppInfo, LocalGitUrl),
    rebar_git_resource:lock(AppInfo2, undefined).

download(TmpDir, AppInfo, State, #{local_git_url:=LocalGitUrl}) ->
    AppInfo2 = update_app_info(AppInfo, LocalGitUrl),
    Source = rebar_app_info:source(AppInfo2),
    case Source of
        {git_subdir, _Url, _Checkout, _Dir} ->
            rebar_git_subdir_resource:download(TmpDir, AppInfo2, State, undefined);
        _ ->
            rebar_git_resource:download(TmpDir, AppInfo2, State, undefined)
    end.


needs_update(AppInfo, #{local_git_url:=LocalGitUrl}) ->
    AppInfo2 = update_app_info(AppInfo, LocalGitUrl),
    rebar_git_resource:needs_update(AppInfo2, undefined).

make_vsn(AppInfo, _CustomState) ->
    rebar_git_resource:make_vsn(AppInfo, undefined).


update_app_info(AppInfo, LocalGitUrl) ->
    Source = get_resource(AppInfo, LocalGitUrl),
    rebar_app_info:source(AppInfo, Source).

get_resource(AppInfo, LocalGitUrl) ->
    Source = rebar_app_info:source(AppInfo),
    ensure_source(Source, LocalGitUrl).

ensure_source({pkg, PkgName, Vsn, _OldHash, _Hash, _}, LocalDepUrl) ->
    ensure_source({git, erlang:binary_to_list(<<PkgName/binary, ".git">>), {tag, Vsn}}, LocalDepUrl);
ensure_source({git_subdir, Url, Checkout, Dir}, LocalDepUrl) ->
    Url2 = ensure_my_dep_dir(Url, LocalDepUrl),
    {git_subdir, Url2, Checkout, Dir};
ensure_source(Source, LocalDepUrl) when is_tuple(Source) ->
    case element(1, Source) of
        git ->
            update_url(Source, LocalDepUrl);
        hg ->
            update_url(Source, LocalDepUrl);
        _ ->
            Source
    end;
ensure_source(Source, _LocalDepUrl) ->
    Source.

update_url(Source, LocalDepUrl) ->
    Url = element(2, Source),
    Url2 = ensure_my_dep_dir(Url, LocalDepUrl),
    setelement(2, Source, Url2).

ensure_my_dep_dir(Url, LocalDepUrl) ->
    Basename = filename:basename(Url),
    lists:append(LocalDepUrl, Basename).
