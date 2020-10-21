rebar3_local_deps
=====

A rebar3 plugin for fetching all dependencies from a local git repository.

Use
-----
```erlang
{plugins, [
    {rebar3_local_deps, ".*",
       {git, "git://github.com/yidayoung/rebar3_local_deps.git", {branch, "master"}}}
]}.
```
maybe you should download also rebar3_local_deps to your local git server, and replace git url  
like this
```erlang
{plugins, [
    {rebar3_local_deps, ".*",
       {git, "http://127.0.0.1:9001/r/rebar3_local_deps.git", {branch, "master"}}}
]}.
```    
add local_git_url in your top rebar.config
```
{local_deps_url, "http://127.0.0.1:9001/r/"}.
{use_local_deps, true}.
```
notice url contain last `/`

then run `rebar3 compile`
all deps include in your project will get from local_deps_url

what define deps is only name and version
all deps will trans to`${local_deps_url}${deps_name}.git`
like ct will trans to http://127.0.0.1:9001/r/ct.git

as said you must ensure your local git contains all deps you need.
you and get git by commond like `git clone --bare xxxxx.git` with `gitbit`

Multiple projects
-----
if you have more than one project, and some of them need local deps, some not.  
or deps url may change.  

you should add those in your `~/.config/rebar3/rebar.config`
```erlang
{local_deps_url, "http://127.0.0.1:9001/r/"}.
```

and in your project `rebar.config`, only need
```erlang
{plugins, [
    {rebar3_local_deps, ".*",
       {git, "http://127.0.0.1:9001/r/rebar3_local_deps.git", {branch, "master"}}}
]}.
{use_local_deps, true}.
```

also you can also move plugins config into user rebar.config