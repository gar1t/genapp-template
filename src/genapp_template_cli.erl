-module(genapp_template_cli).

-export([run/1]).

-define(PROG, "genapp-template").

run(Args) ->
    setup_temp_tracing(),
    {Template, Metadata, Target} = parse_args(Args),
    genapp_template:write_template(Template, Metadata, Target).

setup_temp_tracing() ->
    genapp_debug:trace_module(resource_database).

parse_args(["-h"]) ->
    usage(0);
parse_args([Template, Metadata, Target]) ->
    {Template, Metadata, Target};
parse_args(_) ->
    usage(1).

usage(Exit) ->
    io:format("usage: ~s template metadata target\n", [?PROG]),
    erlang:halt(Exit).

