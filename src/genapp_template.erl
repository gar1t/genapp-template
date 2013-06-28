-module(genapp_template).
-include_lib("eunit/include/eunit.hrl").
-export([write_template/2]).

write_template(Template, Filename) when is_list(Template) ->
    Metadata = json_to_proplist(file, "/tmp/metadata.json"),
    Content = compile_template(Template, parse_metadata(Metadata)),
    ok = file:write_file(Filename, Content).

compile_template(Template, Vars) when is_list(Template) ->
    Mod = template_module(Template),
    ok = erlydtl:compile(Template, Mod),
    {ok, Content} = Mod:render(Vars),
    Content.

template_module(Template) ->
    Hash = erlang:phash2(Template, 10000000),
    list_to_atom("template_" ++ integer_to_list(Hash)).

json_to_proplist(file, Filename) ->
    {ok, Json} = file:read_file(Filename),
    json_to_proplist(jiffy:decode(Json)).
json_to_proplist({L}) ->
    json_to_proplist(L);
json_to_proplist(L) when is_list(L) ->
    [json_to_proplist(X) || X <- L];
json_to_proplist({Name, Val}) ->
    {Name, json_to_proplist(Val)};
json_to_proplist(X) -> X.

parse_metadata(Metadata) ->
    parse_metadata(Metadata, [{<<"metadata">>, Metadata}]).
parse_metadata([ {_, ResourceDef} | T ], ParsedMetadata) ->
    FormattedDefinition = format_resource(ResourceDef),
    parse_metadata(T, merge_resource(FormattedDefinition, ParsedMetadata));
parse_metadata([], ParsedMetadata) ->
    io:format({ParsedMetadata}),
    ParsedMetadata.

format_resource(Definition) ->
    ResourceTypeTuple = lists:keyfind(<<"__resource_type__">>, 1, Definition),
    CleanDefinition = lists:keydelete(<<"__resource_type__">>, 1, Definition), 
    format_resource(ResourceTypeTuple, CleanDefinition).

format_resource({_, <<"database">>}, Definition) ->
    ReplaceKeys = [{<<"DATABASE_URL">>, <<"url">>},
                   {<<"DATABASE_USERNAME">>, <<"username">>},
                   {<<"DATABASE_PASSWORD">>, <<"password">>},
                   {<<"__resource_name__">>, <<"name">>}],
    {Formatted, Rest} = split_and_replace_keys(Definition, ReplaceKeys),
    MergedDefinition = merge_configuration(Formatted, Rest),
    {<<"url">>, Url} = lists:keyfind(<<"url">>, 1, MergedDefinition),
    UrlDerivatives = url_properties(Url),
    {<<"database">>, lists:append([UrlDerivatives, MergedDefinition])};
format_resource({_, <<"email">>}, Definition) ->
    ReplaceKeys = [{<<"SENDGRID_SMTP_HOST">>, <<"host">>},
                   {<<"SENDGRID_USERNAME">>, <<"username">>},
                   {<<"SENDGRID_PASSWORD">>, <<"password">>},
                   {<<"__resource_name__">>, <<"name">>}],
    {Formatted, Rest} = split_and_replace_keys(Definition, ReplaceKeys),
    {<<"email">>, merge_configuration(Formatted, Rest)};
format_resource({_, <<"session-store">>}, Definition) ->
    {<<"servers">>, Servers} = lists:keyfind(<<"servers">>, 1, Definition),
    ServerStringList = string:tokens(binary_to_list(Servers), ","),
    ServerList = lists:map(fun(L) -> list_to_binary(L) end, ServerStringList),
    CleanDefinition = lists:keydelete(<<"servers">>, 1, Definition),
    NewDefinition = lists:append([CleanDefinition, [{<<"servers">>, ServerList}]]),
    ReplaceKeys = [{<<"__resource_name__">>, <<"name">>}],
    {Formatted, Rest} = split_and_replace_keys(NewDefinition, ReplaceKeys),
    {<<"session-store">>, lists:append([Formatted, Rest])};
format_resource({_, _}, _) ->
    false;
format_resource(false, _) ->
    false.

merge_configuration(Formatted, []) ->
    Formatted;
merge_configuration(Formatted, Rest) ->
    lists:append([Formatted, [{<<"configuration">>, Rest}]]).

split_and_replace_keys(Definiton, ReplaceKeys) ->
    split_and_replace_keys(Definiton, ReplaceKeys, []).   
split_and_replace_keys(Rest, [ {OldKey, NewKey} | T ], Formatted) ->
    {OldKey, Value} = lists:keyfind(OldKey, 1, Rest),
    NewRest = lists:keydelete(OldKey, 1, Rest),
    NewFormatted = lists:append([Formatted, [{NewKey, Value}]]),
    split_and_replace_keys(NewRest, T, NewFormatted); 
split_and_replace_keys(Rest, [], Formatted) ->
    {Formatted, Rest}.
    
url_properties(Url) ->
    {Type, Address, Port, Database} = parse_url(Url),
    {DriverName, DriverClass} = java_driver(Type),
    [{<<"type">>, Type},
    {<<"address">>, Address},
    {<<"port">>, Port},
    {<<"database">>, Database},
    {<<"driverName">>, DriverName},
    {<<"driverClass">>, DriverClass}].

parse_url(Url) ->
    Opts = [{return, binary}],
    {ok, TypeRegexp} = re:compile(":\/\/.*$"),
    Type = re:replace(Url, TypeRegexp, "", Opts), 
    HostRegexpLeftString = "^" ++ binary_to_list(Type) ++ ":\/\/",
    {ok, HostRegexpLeft} = re:compile(HostRegexpLeftString),
    {ok, HostRegexpRight} = re:compile(":[0-9]*\/.*$"),
    PartialHost = re:replace(Url, HostRegexpLeft, "", Opts),
    Host = re:replace(PartialHost, HostRegexpRight, "", Opts),
    PortRegexpLeftString = HostRegexpLeftString ++ binary_to_list(Host) ++ ":",
    {ok, PortRegexpLeft} = re:compile(PortRegexpLeftString),
    {ok, PortRegexpRight} = re:compile("\/.*$"),
    PartialPort = re:replace(Url, PortRegexpLeft, "", Opts),
    Port = re:replace(PartialPort, PortRegexpRight, "", Opts),
    DatabaseRegexpString = PortRegexpLeftString ++ binary_to_list(Port) ++ "\/",
    {ok, DatabaseRegexp} = re:compile(DatabaseRegexpString),
    Database = re:replace(Url, DatabaseRegexp, "", Opts),
    {Type, Host, Port, Database}.

java_driver(<<"mysql">>) ->
    {<<"com.mysql.jdbc.Driver">>, 
    <<"com.mysql.jdbc.jdbc2.optional.MysqlDataSource">>};
java_driver(<<"sql">>) ->
    {<<"com.microsoft.sqlserver.jdbc.SQLServerDriver">>, 
    <<"com.microsoft.sqlserver.jdbc.SQLServerDataSource">>};
java_driver(<<"postgres">>) ->
    {<<"org.postgresql.Driver">>, 
    <<"org.postgresql.ds.PGSimpleDataSource">>};
java_driver(<<"oracle">>) ->
    {<<"oracle.jdbc.OracleDriver">>,
    <<"oracle.jdbc.pool.OracleDataSource">>}.

merge_resource(false, ParsedMetadata) ->
    ParsedMetadata;
merge_resource({ResourceType, ResourcePropList}, ParsedMetadata) ->
    CurrentData = lists:keyfind(ResourceType, 1, ParsedMetadata),
    merge_resource(CurrentData, ResourceType, ResourcePropList, ParsedMetadata).
merge_resource(false, ResourceType, ResourcePropList, ParsedMetadata) ->
    lists:append([ParsedMetadata, [{ResourceType, [{ResourcePropList}]}]]);
merge_resource({ResourceType, CurrentList}, ResourceType, 
                ResourcePropList, ParsedMetadata) ->
    MergedList = lists:append([CurrentList, [{ResourcePropList}]]),
    PrunedMetadata = lists:keydelete(ResourceType, 1, ParsedMetadata),
    lists:append([[{ResourceType, MergedList}], PrunedMetadata]).

json_to_proplist_test() ->
    P = fun(Str) -> json_to_proplist(jiffy:decode(Str)) end,
    ?assertEqual(1, P("1")),
    ?assertEqual([], P("{}")),
    ?assertEqual([{<<"foo">>, 1}], P("{\"foo\": 1}")),
    ?assertEqual([{<<"foo">>, <<"FOO">>}], P("{\"foo\": \"FOO\"}")),
    ?assertEqual(
       [{<<"foo">>, [{<<"bar">>, <<"BAR">>}]}],
       P("{\"foo\": {\"bar\": \"BAR\"}}")),
    ?assertEqual([], P("[]")),
    ?assertEqual([1, 2, 3], P("[1, 2, 3]")),
    ?assertEqual([1, <<"foo">>], P("[1, \"foo\"]")),
    ?assertEqual(
       [1, [{<<"foo">>, <<"FOO">>}]],
       P("[1, {\"foo\": \"FOO\"}]")),
    ok.