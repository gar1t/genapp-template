-module(genapp_template).
-export([write_template/2]).

write_template(Template, Filename) when is_list(Template) ->
    {ok, Json} = file:read_file("/tmp/metadata.json"),
    Parsed = json_to_proplist(Json),
    Content = compile_template(Template, Parsed),
    ok = file:write_file(Filename, Content).

compile_template(Template, Vars) when is_list(Template) ->
    Mod = template_module(Template),
    ok = erlydtl:compile(Template, Mod),
    {ok, Content} = Mod:render(Vars),
    Content.

json_to_proplist({L}) ->
    json_to_proplist(L);
json_to_proplist(L) when is_list(L) ->
    [json_to_proplist(X) || X <- L];
json_to_proplist({Name, Val}) ->
    {Name, json_to_proplist(Val)};
json_to_proplist(X) -> X.

template_module(Template) ->
    Hash = erlang:phash2(Template, 10000000),
    list_to_atom("template_" ++ integer_to_list(Hash)).

-include_lib("eunit/include/eunit.hrl").

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
