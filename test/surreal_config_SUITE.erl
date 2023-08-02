-module(surreal_config_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([simple_conn_uri/1, secure_conn_uri/1, custom_timeout_uri/1]).

all() ->
    [simple_conn_uri, secure_conn_uri, custom_timeout_uri].

simple_conn_uri(_Config) ->
    Uri = "surrealdb://root:root@localhost:8000/ns/db",
    Result = surreal_config:parse(Uri),

    Expected =
        {ok, #{
            host => "localhost",
            username => "root",
            password => "root",
            namespace => "ns",
            database => "db",
            port => 8000,
            tls => false,
            timeout => 5000
        }},
    ?assertEqual(Expected, Result).

secure_conn_uri(_Config) ->
    Uri = "surrealdb+tls://root:root@localhost:8000/ns/db",
    Result = surreal_config:parse(Uri),

    Expected =
        {ok, #{
            host => "localhost",
            username => "root",
            password => "root",
            namespace => "ns",
            database => "db",
            port => 8000,
            tls => true,
            timeout => 5000
        }},
    ?assertEqual(Expected, Result).

custom_timeout_uri(_Config) ->
    Uri = "surrealdb://root:root@localhost:8000/ns/db?timeout=1000",
    Result = surreal_config:parse(Uri),

    Expected =
        {ok, #{
            host => "localhost",
            username => "root",
            password => "root",
            namespace => "ns",
            database => "db",
            port => 8000,
            tls => false,
            timeout => 1000
        }},
    ?assertEqual(Expected, Result).
