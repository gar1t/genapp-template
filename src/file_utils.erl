-module(file_utils).

-export([json_to_proplist/2, compile_template/3]).

compile_template(Template, Vars, Target) when is_list(Template) ->
    Mod = template_module(Template),
    %% TODO Find what's wrong here
    Options = [{custom_filters_modules, [custom_filters]}],
    {ok, _} = erlydtl:compile(Template, Mod, Options),
    {ok, Content} = Mod:render(Vars),
    write(Content, Target).

write(Bin, stdout) ->
    io:format("~s~n", [Bin]);
write(Bin, Filename) ->
    ok = file:write_file(Filename, Bin).

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
