-module(resource_utils).
-export([split_and_replace_keys/2, format_resource/1]).

format_resource(Definition) ->
    ResourceTypeTuple = lists:keyfind(<<"__resource_type__">>, 1, Definition),
    CleanDefinition = lists:keydelete(<<"__resource_type__">>, 1, Definition), 
    format_resource(ResourceTypeTuple, CleanDefinition).
format_resource({_, <<"database">>}, Definition) ->
    {<<"database">>, resource_database:parse_resource(Definition)};
format_resource({_, <<"email">>}, Definition) ->
    {<<"email">>, resource_email:parse_resource(Definition)};
format_resource({_, <<"session-store">>}, Definition) ->
    {<<"session-store">>, resource_session:parse_resource(Definition)};
format_resource({_, _}, _) ->
    false;
format_resource(false, _) ->
    false.

split_and_replace_keys(Definiton, KnownKeys) ->
    split_and_replace_keys(Definiton, KnownKeys, []).   
split_and_replace_keys(Rest, [ {OldKey, NewKey} | T ], Filtered) ->
    {OldKey, Value} = lists:keyfind(OldKey, 1, Rest),
    NewRest = lists:keydelete(OldKey, 1, Rest),
    NewFiltered = lists:append([Filtered, [{NewKey, Value}]]),
    split_and_replace_keys(NewRest, T, NewFiltered); 
split_and_replace_keys(Rest, [ {Key} | T ], Filtered) ->
    split_and_replace_keys(Rest, [ {Key, Key} | T ], Filtered); 
split_and_replace_keys(Rest, [], Filtered) ->
    merge_configuration(Filtered, Rest).

merge_configuration(Filtered, []) ->
    Filtered;
merge_configuration(Filtered, Rest) ->
    lists:append([Filtered, [{<<"configuration">>, Rest}]]).
