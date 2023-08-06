-module(surreal_config_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    simple_conn/1,
    secure_conn/1,
    custom_timeout/1,
    invalid_uri/1,
    invalid_scheme/1,
    invalid_userinfo/1,
    invalid_path/1,
    invalid_query/1,
    invalid_timeout/1
]).

all() ->
    [
        simple_conn,
        secure_conn,
        custom_timeout,
        invalid_uri,
        invalid_scheme,
        invalid_userinfo,
        invalid_path,
        invalid_query,
        invalid_timeout
    ].

simple_conn(_Config) ->
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

secure_conn(_Config) ->
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

custom_timeout(_Config) ->
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

invalid_uri(_Config) ->
    Uri = "surreal://root:root@localhost",
    Result = surreal_config:parse(Uri),

    Expected = {error, invalid_uri, []},
    ?assertEqual(Expected, Result).

invalid_scheme(_Config) ->
    Uri = "surreal://root:root@localhost:8000/ns/db",
    Result = surreal_config:parse(Uri),

    Expected = {error, invalid_scheme},
    ?assertEqual(Expected, Result).

invalid_userinfo(_Config) ->
    Uri = "surrealdb+tls://invalid@localhost:8000/ns/db",
    Result = surreal_config:parse(Uri),

    Expected = {error, invalid_userinfo},
    ?assertEqual(Expected, Result).

invalid_path(_Config) ->
    Uri = "surrealdb+tls://root:root@localhost:8000/ns",
    Result = surreal_config:parse(Uri),

    Expected = {error, invalid_path},
    ?assertEqual(Expected, Result).

invalid_query(_Config) ->
    Uri = "surrealdb+tls://root:root@localhost:8000/ns/db?time=5000",
    Result = surreal_config:parse(Uri),

    Expected = {error, invalid_query},
    ?assertEqual(Expected, Result).

invalid_timeout(_Config) ->
    Uri = "surrealdb+tls://root:root@localhost:8000/ns/db?timeout=5ms",
    Result = surreal_config:parse(Uri),

    Expected = {error, invalid_timeout},
    ?assertEqual(Expected, Result).
