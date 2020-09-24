rebar3_local_deps
=====

A rebar3 plugin for fetching all dependencies from a local git repository.

Use
-----
```erlang
{plugins, [
    {rebar3_svn_deps, ".*",
       {git, "git://github.com/yidayoung/rebar3_local_deps.git", {branch, "master"}}}
]}.
```
    
add local_git_url in your top rebar.config
```
{local_deps_url, "http://127.0.0.1:9001/r/"}.
```
notice url contain last `/`

then run `rebar3 compile`
all deps include in your project will get from local_deps_url

what define deps is only name and version
all deps will trans to`${local_deps_url}${deps_name}.git`
like ct will trans to http://127.0.0.1:9001/r/ct.git

as said you must ensure your local git contains all deps you need.
you and get git by commond like `git clone --bare xxxxx.git` with `gitbit`