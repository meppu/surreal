-module(surreal_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_group/2, end_per_group/2]).

-export([
    users_create/1,
    users_update/1,
    users_merge/1,
    users_patch/1,
    users_select/1,
    users_patch2/1,
    users_delete/1,
    users_insert/1,
    users_query/1
]).

-export([
    connectivity_ping/1,
    connectivity_query/1,
    connectivity_signup/1,
    connectivity_signin/1,
    connectivity_invalidate/1,
    connectivity_authenticate/1,
    connectivity_unset/1,
    connectivity_use/1,
    connectivity_signin2/1
]).

all() ->
    [{group, users}, {group, connectivity}].

groups() ->
    [
        {users, [sequence], [
            users_create,
            users_update,
            users_merge,
            users_patch,
            users_select,
            users_patch2,
            users_delete,
            users_insert,
            users_query
        ]},
        {connectivity, [sequence], [
            connectivity_ping,
            connectivity_query,
            connectivity_signup,
            connectivity_signin,
            connectivity_invalidate,
            connectivity_authenticate,
            connectivity_unset,
            connectivity_use,
            connectivity_signin2
        ]}
    ].

init_per_group(GroupName, Config) ->
    Uri = io_lib:format("surrealdb://root:root@localhost:8000/~s/test", [GroupName]),

    {ok, Pid} = surreal:start_link(Uri, GroupName),
    unlink(Pid),

    [{db, Pid} | Config].

end_per_group(GroupName, Config) ->
    Pid = ?config(db, Config),

    Query = io_lib:format(
        "REMOVE TABLE users;"
        "REMOVE SCOPE test;"
        "REMOVE DATABASE test;"
        "REMOVE NAMESPACE ~s;",
        [GroupName]
    ),

    surreal:query(Pid, Query),

    surreal:close(Pid).

%%%==========================================================================
%%%
%%%   Users group
%%%
%%%==========================================================================

users_create(Config) ->
    Pid = ?config(db, Config),

    Data = #{<<"age">> => 16},
    Response = surreal:create(Pid, "users:meppu", Data),

    Expected = {ok, #{<<"id">> => <<"users:meppu">>, <<"age">> => 16}},
    ?assertEqual(Expected, Response).

users_update(Config) ->
    Pid = ?config(db, Config),

    NewData = #{<<"age">> => 18},
    Response = surreal:update(Pid, "users:meppu", NewData),

    Expected = {ok, #{<<"id">> => <<"users:meppu">>, <<"age">> => 18}},
    ?assertEqual(Expected, Response).

users_merge(Config) ->
    Pid = ?config(db, Config),

    MergeData = #{<<"verified">> => true, <<"other">> => <<"test">>},
    Response = surreal:merge(Pid, "users:meppu", MergeData),

    Expected =
        {ok, #{
            <<"id">> => <<"users:meppu">>,
            <<"age">> => 18,
            <<"verified">> => true,
            <<"other">> => <<"test">>
        }},
    ?assertEqual(Expected, Response).

users_patch(Config) ->
    Pid = ?config(db, Config),

    PatchData = [
        {add, "/extra", true},
        {remove, "/verified"},
        {replace, "/age", 16},
        {diff, "/other", <<"@@ -1,4 +1,4 @@\n te\n-s\n+x\n t\n">>}
    ],

    {ok, Patches} = surreal:patch(Pid, "users:meppu", PatchData),
    ?assert(is_list(Patches)).

users_select(Config) ->
    Pid = ?config(db, Config),

    Response = surreal:select(Pid, "users:meppu"),

    Expected =
        {ok, #{
            <<"id">> => <<"users:meppu">>,
            <<"age">> => 16,
            <<"extra">> => true,
            <<"other">> => <<"text">>
        }},
    ?assertEqual(Expected, Response).

users_patch2(Config) ->
    Pid = ?config(db, Config),

    PatchData = [
        {remove, "/extra"},
        {remove, "/other"}
    ],

    {ok, Patches} = surreal:patch(Pid, "users:meppu", PatchData),
    ?assert(is_list(Patches)).

users_delete(Config) ->
    Pid = ?config(db, Config),

    Response = surreal:delete(Pid, "users:meppu"),

    Expected = {ok, #{<<"id">> => <<"users:meppu">>, <<"age">> => 16}},
    ?assertEqual(Expected, Response).

users_insert(Config) ->
    Pid = ?config(db, Config),

    Data = [
        #{
            <<"id">> => <<"users:meppu">>,
            <<"age">> => 16
        },
        #{
            <<"id">> => <<"users:gulce">>,
            <<"age">> => 17
        },
        #{
            <<"id">> => <<"users:tuhana">>,
            <<"age">> => 19
        }
    ],
    Response = surreal:insert(Pid, "users", Data),

    Expected = {ok, Data},
    ?assertEqual(Expected, Response).

users_query(Config) ->
    Pid = ?config(db, Config),

    Response = surreal:query(Pid, "SELECT VALUE id FROM users WHERE age > $age", #{
        <<"age">> => 16
    }),

    Expected = [{ok, [<<"users:gulce">>, <<"users:tuhana">>]}],
    ?assertEqual(Expected, Response).

%%%==========================================================================
%%%
%%%   Connectivity group
%%%
%%%==========================================================================

connectivity_ping(Config) ->
    Pid = ?config(db, Config),

    Response = surreal:ping(Pid),
    Expected = {ok, null},
    ?assertEqual(Expected, Response).

connectivity_query(Config) ->
    Pid = ?config(db, Config),

    ScopeQuery =
        "DEFINE SCOPE test SESSION 1d"
        "\nSIGNUP (CREATE user SET user = $user, pass = crypto::argon2::generate($pass))"
        "\nSIGNIN (SELECT * FROM user WHERE user = $user AND crypto::argon2::compare(pass, $pass))",

    Response = surreal:query(Pid, ScopeQuery),
    ?assertEqual([{ok, null}], Response).

connectivity_signup(Config) ->
    Pid = ?config(db, Config),

    User = #{
        <<"NS">> => <<"connectivity">>,
        <<"DB">> => <<"test">>,
        <<"SC">> => <<"test">>,
        <<"user">> => <<"tuhana">>,
        <<"pass">> => <<"kitten">>
    },

    {ok, Token} = surreal:signup(Pid, User),
    ?assert(is_binary(Token)).

connectivity_signin(Config) ->
    Pid = ?config(db, Config),

    User = #{
        <<"NS">> => <<"connectivity">>,
        <<"DB">> => <<"test">>,
        <<"SC">> => <<"test">>,
        <<"user">> => <<"tuhana">>,
        <<"pass">> => <<"kitten">>
    },

    {ok, Token} = surreal:signin(Pid, User),
    ?assert(is_binary(Token)),

    Response = surreal:set(Pid, "auth_token", Token),
    ?assertEqual({ok, null}, Response).

connectivity_invalidate(Config) ->
    Pid = ?config(db, Config),

    Response = surreal:invalidate(Pid),
    ?assertEqual({ok, null}, Response).

connectivity_authenticate(Config) ->
    Pid = ?config(db, Config),

    [{ok, [Token]}] = surreal:query(Pid, "SELECT * FROM $auth_token"),
    ?assert(is_binary(Token)),

    Response = surreal:authenticate(Pid, Token),
    ?assertEqual({ok, null}, Response).

connectivity_unset(Config) ->
    Pid = ?config(db, Config),

    Response = surreal:unset(Pid, "auth_token"),
    ?assertEqual({ok, null}, Response).

connectivity_use(Config) ->
    Pid = ?config(db, Config),

    Response = surreal:use(Pid, "connectivity", "test"),
    ?assertEqual({ok, null}, Response).

connectivity_signin2(Config) ->
    Pid = ?config(db, Config),

    {ok, Token} = surreal:signin(Pid, "root", "root"),
    ?assert(is_binary(Token)).
