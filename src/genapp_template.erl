-module(genapp_template).

-export([write_template/2, write_template/3]).

write_template(Template, Filename) ->
    Parsed = parse_metadata({get_metadata()}),
    file_utils:compile_template(Template, Parsed, Filename).

write_template(Template, Filename, OptionsFilename) ->
    Options = file_utils:json_to_proplist(file, OptionsFilename),
    Parsed = parse_metadata({get_metadata(), Options}),
    file_utils:compile_template(Template, Parsed, Filename).

get_metadata() ->
    MetadataFile = os:getenv("genapp_dir") ++  "/metadata.json",
    file_utils:json_to_proplist(file, MetadataFile).

parse_metadata({Metadata}) ->
    parse_metadata(Metadata, [{<<"metadata">>, Metadata}]);
parse_metadata({Metadata, Options}) ->
    parse_metadata(Metadata,
        [{<<"metadata">>, Metadata},
        {<<"stack_opts">>, Options}]).

parse_metadata([ {_, ResourceDef} | T ], ParsedMetadata) ->
    FormattedDefinition = resource_utils:format_resource(ResourceDef),
    parse_metadata(T, merge_resource(FormattedDefinition, ParsedMetadata));
parse_metadata([], ParsedMetadata) ->
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
