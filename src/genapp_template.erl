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
    ParsedMetadata.

format_resource(Definition) ->
    ResourceTypeTuple = lists:keyfind(<<"__resource_type__">>, 1, Definition), 
    format_resource(ResourceTypeTuple, Definition).
format_resource(false, _) ->
    false;
format_resource({_, ResourceType} , Definiton) ->
    {ResourceType, Definiton}.

merge_resource(false, ParsedMetadata) ->
    ParsedMetadata;
merge_resource({ResourceType, ResourcePropList}, ParsedMetadata) ->
    CurrentData = lists:keyfind(ResourceType, 1, ParsedMetadata),
    merge_resource(CurrentData, ResourceType, ResourcePropList, ParsedMetadata).
merge_resource(false, ResourceType, ResourcePropList, ParsedMetadata) ->
    lists:append([ParsedMetadata, [{ResourceType, [{ResourcePropList}]}]]);
merge_resource({ResourceType, CurrentList}, ResourceType, ResourcePropList, ParsedMetadata) ->
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