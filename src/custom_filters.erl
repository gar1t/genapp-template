-module(custom_filters).

-export([databases/1, smtp_servers/1, session_stores/1]).

databases(Metadata) ->
	resource_utils:filter_metadata(Metadata, <<"database">>).

smtp_servers(Metadata) ->
	resource_utils:filter_metadata(Metadata, <<"email">>).

session_stores(Metadata) ->
	resource_utils:filter_metadata(Metadata, <<"session-store">>).
