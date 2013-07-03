-module(resource_utils).

-export([split_and_replace_keys/2, filter_metadata/1]).

filter_metadata(Metadata, ResourceType) ->
    filter_metadata(Metadata, ResourceType, []).

filter_metadata([ {_, Definition} | T ], ResourceType, ParsedMetadata) ->
    ParsedDefinition = resource_utils:filter_resource(ResourceType, Definition),
    filter_metadata(
        T, ResourceType, merge_resource(ParsedDefinition, ParsedMetadata));
filter_metadata([],_ , ParsedMetadata) ->
    ParsedMetadata.

merge_resource(false, ParsedMetadata) ->
    ParsedMetadata;
merge_resource({ResourceType, ResourcePropList}, ParsedMetadata) ->
    CurrentData = lists:keyfind(ResourceType, 1, ParsedMetadata),
    merge_resource(
      CurrentData, ResourceType, ResourcePropList, ParsedMetadata).

merge_resource(false, ResourceType, ResourcePropList, ParsedMetadata) ->
    lists:append([ParsedMetadata, [{ResourceType, [{ResourcePropList}]}]]);
merge_resource({ResourceType, CurrentList}, ResourceType,
                ResourcePropList, ParsedMetadata) ->
    MergedList = lists:append([CurrentList, [{ResourcePropList}]]),
    PrunedMetadata = lists:keydelete(ResourceType, 1, ParsedMetadata),
    lists:append([[{ResourceType, MergedList}], PrunedMetadata]).

filter_resource(ResourceType, Definition) ->
    ResourceTypeTuple = lists:keyfind(<<"__resource_type__">>, 1, Definition),
    CleanDefinition = lists:keydelete(<<"__resource_type__">>, 1, Definition),
    filter_resource(ResourceTypeTuple, ResourceType, CleanDefinition).

filter_resource({<<"__resource_type__">>, ResourceType}, ResourceType, Definition) ->
    format_resource({<<"__resource_type__">>, ResourceType}, Definition);
filter_resource({<<"__resource_type__">>, _}, _, _) ->
    false;
filter_resource(false,_ ,_) ->
    false.

format_resource({<<"__resource_type__">>, <<"database">>}, Definition) ->
    {<<"database">>, resource_database:parse_resource(Definition)};
format_resource({<<"__resource_type__">>, <<"email">>}, Definition) ->
    {<<"email">>, resource_email:parse_resource(Definition)};
format_resource({<<"__resource_type__">>, <<"session-store">>}, Definition) ->
    {<<"session_store">>, resource_session:parse_resource(Definition)};
format_resource({<<"__resource_type__">>, _}, _) ->
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
