-module(custom_filters).

-export([resource_type/2, databases/1, smtp_servers/1, session_stores/1]).

resource_type(Metadata, Type) ->
    filter_resource_type(Metadata, Type, []).

filter_resource_type([{Name, Section}|Rest], Type, Acc) ->
    IsType = section_is_type(Section, Type),
    filter_resource_type(
      Rest, Type, maybe_acc(IsType, {Name, Section}, Acc));
filter_resource_type([], _Type, Acc) ->
    lists:reverse(Acc).

section_is_type(Section, Type) ->
    proplists:get_value(<<"__resource_type__">>, Section) == Type.

maybe_acc(true, Item, Acc) -> [Item|Acc];
maybe_acc(false, _Item, Acc) -> Acc.

databases(Metadata) ->
    resource_utils:filter_metadata(Metadata, <<"database">>).

smtp_servers(Metadata) ->
    resource_utils:filter_metadata(Metadata, <<"email">>).

session_stores(Metadata) ->
    resource_utils:filter_metadata(Metadata, <<"session-store">>).
