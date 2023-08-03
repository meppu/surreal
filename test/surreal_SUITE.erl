-module(surreal_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-export([
    add_user/1,
    update_user/1,
    merge_user/1,
    patch_user/1,
    select_user/1,
    delete_user/1,
    insert_user/1,
    query_user/1
]).

all() ->
    [{group, users}].

groups() ->
    [
        {users, [sequence], [
            add_user,
            update_user,
            merge_user,
            patch_user,
            select_user,
            delete_user,
            insert_user,
            query_user
        ]}
    ].

init_per_group(GroupName, Config) ->
    Uri = io_lib:format("surrealdb://root:root@localhost:8000/~s/test", [GroupName]),

    {ok, Pid} = surreal:start_link(Uri, GroupName),
    unlink(Pid),

    [{db, Pid} | Config].

end_per_group(_GroupName, Config) ->
    Pid = ?config(db, Config),

    surreal:delete(Pid, "users"),
    surreal:close(Pid).

%%%==========================================================================
%%%
%%%   Users group
%%%
%%%==========================================================================

add_user(Config) ->
    Pid = ?config(db, Config),

    Data = #{<<"age">> => 16},
    Response = surreal:create(Pid, "users:meppu", Data),

    Expected = {ok, #{<<"id">> => <<"users:meppu">>, <<"age">> => 16}},
    ?assertEqual(Expected, Response).

update_user(Config) ->
    Pid = ?config(db, Config),

    NewData = #{<<"age">> => 18},
    Response = surreal:update(Pid, "users:meppu", NewData),

    Expected = {ok, #{<<"id">> => <<"users:meppu">>, <<"age">> => 18}},
    ?assertEqual(Expected, Response).

merge_user(Config) ->
    Pid = ?config(db, Config),

    MergeData = #{<<"verified">> => true},
    Response = surreal:merge(Pid, "users:meppu", MergeData),

    Expected = {ok, #{<<"id">> => <<"users:meppu">>, <<"age">> => 18, <<"verified">> => true}},
    ?assertEqual(Expected, Response).

patch_user(Config) ->
    Pid = ?config(db, Config),

    PatchData = [{remove, "/verified"}, {replace, "/age", 16}],
    {ok, Patches} = surreal:patch(Pid, "users:meppu", PatchData),
    ?assert(is_list(Patches)).

select_user(Config) ->
    Pid = ?config(db, Config),

    Response = surreal:select(Pid, "users:meppu"),

    Expected = {ok, #{<<"id">> => <<"users:meppu">>, <<"age">> => 16}},
    ?assertEqual(Expected, Response).

delete_user(Config) ->
    Pid = ?config(db, Config),

    Response = surreal:delete(Pid, "users:meppu"),

    Expected = {ok, #{<<"id">> => <<"users:meppu">>, <<"age">> => 16}},
    ?assertEqual(Expected, Response).

insert_user(Config) ->
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

query_user(Config) ->
    Pid = ?config(db, Config),

    Response = surreal:query(Pid, "SELECT VALUE id FROM users WHERE age > $age", #{
        <<"age">> => 16
    }),

    Expected = [{ok, [<<"users:gulce">>, <<"users:tuhana">>]}],
    ?assertEqual(Expected, Response).
