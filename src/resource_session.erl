-module(resource_session).

-export([parse_resource/1]).

parse_resource(Definition) ->
    {<<"servers">>, Servers} = lists:keyfind(<<"servers">>, 1, Definition),
    ServerStringList = string:tokens(binary_to_list(Servers), ","),
    ServerList = lists:map(fun(L) -> list_to_binary(L) end, ServerStringList),
    CleanDefinition = lists:keydelete(<<"servers">>, 1, Definition),
    NewDefinition = lists:append([CleanDefinition, [{<<"servers">>, ServerList}]]),
    KnownKeys =
        [{<<"__resource_name__">>, <<"name">>},
         {<<"servers">>},
         {<<"username">>},
         {<<"password">>}],
    resource_utils:split_and_replace_keys(NewDefinition, KnownKeys).
