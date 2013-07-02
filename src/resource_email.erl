-module(resource_email).

-export([parse_resource/1]).

parse_resource(Definition) ->
    KnownKeys =
        [{<<"__resource_name__">>, <<"name">>},
         {<<"SENDGRID_SMTP_HOST">>, <<"host">>},
         {<<"SENDGRID_USERNAME">>, <<"username">>},
         {<<"SENDGRID_PASSWORD">>, <<"password">>}],
    resource_utils:split_and_replace_keys(Definition, KnownKeys).
