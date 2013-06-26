-module(genapp_template).
-export([write_template/2]).

write_template(Template, Filename) when is_list(Template) ->
    {ok, Json} = file:read_file("/tmp/metadata.json"),
    {Vars} = jiffy:decode(Json),
    Parsed = jiffy_to_proplist(Vars),
    Content = compile_template(Template, Parsed),
    ok = file:write_file(Filename, Content).

compile_template(Template, Vars) when is_list(Template) ->
    Mod = template_module(Template),
    ok = erlydtl:compile(Template, Mod),
    {ok, Content} = Mod:render(Vars),
    Content.

% The following section decodes Jiffy output to a proplist

jiffy_to_proplist(JiffyList) ->
    jiffy_to_proplist(JiffyList, []).
jiffy_to_proplist([H|T], Proplist) ->
    NewProplist = format_jiffy_tuplewr(H, Proplist),
    jiffy_to_proplist(T, NewProplist);
jiffy_to_proplist([], Proplist) ->
    Proplist.

format_jiffy_tuple(Element, Proplist) when is_tuple(Element) ->
    {Key, Value} = Element,
    FormattedValue = format_jiffy_tuple_value(Value),
    lists:append([[{Key, FormattedValue}], Proplist]).

format_jiffy_tuple_value(Value) when is_tuple(Value) ->
    {JiffyList} = Value,
    jiffy_to_proplist(JiffyList);
format_jiffy_tuple_value(Value) ->
    Value.

% template_module returns an atom that is linked to a variable.

template_module(Template) ->
    Hash = erlang:phash2(Template, 10000000),
    list_to_atom("template_" ++ integer_to_list(Hash)).
