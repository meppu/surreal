-module(surreal_test).
-include_lib("eunit/include/eunit.hrl").

loader_test() ->
    Config = [
        {name, example_client},
        {host, "localhost"},
        {port, 8000},
        {use, {"test", "lt"}},
        {signin, {"root", "root"}}
    ],

    {ok, Pid} = surreal_config:load(Config),
    surreal:delete(Pid, "testing"),

    Result = surreal:create(example_client, "testing:wow", #{
        <<"name">> => <<"kekw">>
    }),

    Should = {ok, #{<<"id">> => <<"testing:wow">>, <<"name">> => <<"kekw">>}},
    ?assertEqual(Result, Should).

surreal_test() ->
    {ok, Pid} = surreal:start_link("ws://localhost:8000"),
    surreal:use(Pid, "test", "sr"),
    surreal:signin(Pid, "root", "root"),

    Should1And3 = {ok, #{<<"id">> => <<"testing:wow">>, <<"name">> => <<"kekw">>}},
    Should2 = {ok, [#{<<"id">> => <<"testing:wow">>, <<"name">> => <<"kekw">>}]},

    Result1 = surreal:create(Pid, "testing:wow", #{
        <<"name">> => <<"kekw">>
    }),
    ?assertEqual(Result1, Should1And3),

    [Result2] = surreal:query(Pid, "SELECT * FROM testing;"),
    ?assertEqual(Result2, Should2),

    Result3 = surreal:delete(Pid, "testing:wow"),
    ?assertEqual(Result3, Should1And3).

query_test() ->
    Query1 = [
        [
            {use, {test, test}}
        ],
        [
            {select},
            {from, users},
            {where, [{'>', age, 13}, {verified, true}]}
        ]
    ],

    Should1 =
        "USE NS test DB test;\nSELECT * FROM users WHERE age > 13, verified = true;",

    ?assertEqual(surreal_query:make_all(Query1), Should1).
