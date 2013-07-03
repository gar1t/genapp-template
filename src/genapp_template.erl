-module(genapp_template).

-export([write_template/3, write_template/4, json_file_to_proplist/1]).

write_template(Template, Metadata, Target) ->
    Context = parse_context(Metadata),
    file_utils:compile_template(Template, Context, Target).
write_template(Template, Metadata, Target, Options) when is_list(Options) ->
    Context = parse_context(Metadata, Options),
    file_utils:compile_template(Template, Context, Target).
    
parse_context(Metadata) ->
    MetadataProplist = json_file_to_proplist(Metadata),
    [{<<"metadata">>, MetadataProplist}].

parse_context(Metadata, Options) ->
    MetadataProplist = json_file_to_proplist(Metadata),
    append_context_options(Options, [{<<"metadata">>, MetadataProplist}]).

append_context_options([ {Key, Filename} | T ], Context) ->
    OptionProplist = [{Key, json_file_to_proplist(Filename)}],
    NewContext = lists:append([OptionProplist, Context]),
    append_context_options(T, NewContext);
append_context_options([], Context) ->
    Context.

json_file_to_proplist(Filename) ->
    file_utils:json_to_proplist(file, Filename).