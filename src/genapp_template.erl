-module(genapp_template).
-export([write_template/2]).

write_template(Template, Filename) when is_list(Template) ->
    Metadata = file_utils:json_to_proplist(file, "/tmp/metadata.json"),
    Parsed = parse_metadata(Metadata),
    file_utils:compile_template(Template, Parsed, Filename).

parse_metadata(Metadata) ->
    parse_metadata(Metadata, [{<<"metadata">>, Metadata}]).
parse_metadata([ {_, ResourceDef} | T ], ParsedMetadata) ->
    FormattedDefinition = resource_utils:format_resource(ResourceDef),
    parse_metadata(T, merge_resource(FormattedDefinition, ParsedMetadata));
parse_metadata([], ParsedMetadata) ->
    io:format(ParsedMetadata),
    ParsedMetadata.

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

