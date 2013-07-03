#!/usr/bin/env escript
%% -*- erlang -*-

main(Args) ->
    setup_path(),
    genapp_template_cli:run(Args).

setup_path() ->
    add_app_ebin(),
    add_deps_ebin().

add_app_ebin() ->
    code:add_path(ebin_path()).

ebin_path() ->
    filename:join(app_home(), "ebin").

app_home() ->
    filename:join(filename:dirname(escript:script_name()), "..").

add_deps_ebin() ->
    Paths = append_ebin(deps_dirs()),
    lists:foreach(fun code:add_path/1, Paths).

deps_dirs() ->
    filelib:wildcard(filename:join([app_home(), "deps", "*"])).

append_ebin(Dirs) ->
    [filename:join(Dir, "ebin") || Dir <- Dirs].
