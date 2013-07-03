-module(resource_database).

-export([parse_resource/1]).

parse_resource(Definition) ->
    KnownKeys =
	[{<<"DATABASE_URL">>, <<"url">>},
	 {<<"DATABASE_USERNAME">>, <<"username">>},
	 {<<"DATABASE_PASSWORD">>, <<"password">>},
	 {<<"__resource_name__">>, <<"name">>}],
    NewDefinition =
	resource_utils:split_and_replace_keys(Definition,KnownKeys),
    {<<"url">>, Url} = lists:keyfind(<<"url">>, 1, NewDefinition),
    UrlDerivatives = url_properties(Url),
    lists:append([UrlDerivatives, NewDefinition]).

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
    DatabaseRegexpString =
        PortRegexpLeftString ++ binary_to_list(Port) ++ "\/",
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
