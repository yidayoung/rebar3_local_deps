-module(rebar3_local_git_resource).

%% API
-export([init/2, lock/2, download/4, needs_update/2, make_vsn/2]).


init(Type, _State) ->
    Resource = rebar_resource_v2:new(Type, ?MODULE, #{}),
    {ok, Resource}.

lock(AppInfo, LocalGitUrl) ->
    AppInfo2 = update_app_info(AppInfo, LocalGitUrl),
    rebar_git_resource:lock(AppInfo2, undefined).

download(TmpDir, AppInfo, State, LocalGitUrl) ->
    AppInfo2 = update_app_info(AppInfo, LocalGitUrl),
    Source = rebar_app_info:source(AppInfo2),
    case Source of
        {git_subdir, _Url, _Checkout, _Dir} ->
            rebar_git_subdir_resource:download(TmpDir, AppInfo2, State, undefined);
        _ ->
            rebar_git_resource:download(TmpDir, AppInfo2, State, undefined)
    end.


needs_update(AppInfo, LocalGitUrl) ->
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

ensure_source({pkg, PkgName, Vsn, _Hash}, LocalDepUrl) ->
    ensure_source({git, erlang:binary_to_list(<<PkgName/binary, ".git">>), {tag, Vsn}}, LocalDepUrl);
ensure_source({pkg, PkgName, Vsn, _OldHash, _Hash}, LocalDepUrl) ->
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
