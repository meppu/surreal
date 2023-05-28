%%% @doc Query builder for Surreal.
-module(surreal_query).

-export([make_one/1, make_all/1]).

-type string_format() :: binary() | atom() | bitstring().
-type query() ::
    {block, Inside :: list(query())}
    | {select}
    | {select, Field :: string_format()}
    | {from, Target :: string_format()}
    | {from, Targets :: list(string_format())}
    | {where, {Operator :: string_format(), Left :: string_format(), Right :: term()}}
    | {where, {Left :: string_format(), Right :: term()}}
    | {where,
        Conditions :: list(
            {Operator :: string_format(), Left :: string_format(), Right :: term()}
            | {Left :: string_format(), Right :: term()}
        )}
    | {create, Something :: string_format()}
    | {set, {Key :: string_format(), Value :: term()}}
    | {set, list({Key :: string_format(), Value :: term()})}
    | {var, {Key :: string_format(), Value :: term()}}
    | {sleep, Millis :: integer()}
    | {delete, Something :: string_format()}
    | {update, Something :: string_format()}
    | {use, {namespace, Namespace :: string_format()}}
    | {use, {database, Database :: string_format()}}
    | {use, {Namespace :: string_format(), Database :: string_format()}}.

%% Generate a query string from list of queries (list of operations).
-spec make_all(Queries :: list(list(query()))) -> string().
make_all(Queries) ->
    Results = lists:map(fun make_one/1, Queries),
    WithFormatted = lists:join("\n", Results),

    lists:flatten(WithFormatted).

%% Generate a query string from list of operations.
-spec make_one(Query :: list(query())) -> string().
make_one(Query) ->
    Results = lists:map(fun make_one_command/1, Query),
    WithFormatted = lists:join(" ", Results) ++ ";",

    lists:flatten(WithFormatted).

%% @private
%% BLOCK Builders
make_one_command({block, Statements}) when is_list(Statements) ->
    Results = lists:map(fun make_one_command/1, Statements),
    WithFormatted = lists:join(" ", Results),

    io_lib:format("(~s)", [WithFormatted]);
%% SELECT Builders
make_one_command({select}) ->
    "SELECT *";
make_one_command({select, Field}) when is_binary(Field); is_atom(Field); is_bitstring(Field) ->
    io_lib:format("SELECT ~s", [Field]);
make_one_command({select, Fields}) when is_list(Fields) ->
    ToString = lists:map(fun atom_to_list/1, Fields),
    WithFormatted = lists:flatten(lists:join(", ", ToString)),

    io_lib:format("SELECT ~s", [WithFormatted]);
%% FROM Builders
make_one_command({from, Target}) when is_binary(Target); is_atom(Target); is_bitstring(Target) ->
    io_lib:format("FROM ~s", [Target]);
make_one_command({from, Targets}) when is_list(Targets) ->
    ToString = lists:map(fun atom_to_list/1, Targets),
    WithFormatted = lists:flatten(lists:join(", ", ToString)),

    io_lib:format("FROM ~s", [WithFormatted]);
%% WHERE Builders
make_one_command({where, {Operator, Left, Right}}) ->
    io_lib:format("WHERE ~s ~s ~p", [Left, Operator, Right]);
make_one_command({where, {Left, Right}}) ->
    io_lib:format("WHERE ~s = ~p", [Left, Right]);
make_one_command({where, Conditions}) when is_list(Conditions) ->
    NewConditions = lists:map(fun(Condition) -> {where, Condition} end, Conditions),
    WithConcat = lists:map(
        fun(Condition) ->
            "WHERE " ++ Actual = make_one_command(Condition),
            Actual
        end,

        NewConditions
    ),
    WithFormatted = lists:flatten(lists:join(", ", WithConcat)),

    io_lib:format("WHERE ~s", [WithFormatted]);
%% CREATE Builders
make_one_command({create, Something}) ->
    io_lib:format("CREATE ~s", [Something]);
%% SET Builders
make_one_command({set, {Key, Value}}) ->
    io_lib:format("SET ~s = ~p", [Key, Value]);
make_one_command({set, AllKv}) when is_list(AllKv) ->
    NewKv = lists:map(fun(Kv) -> {set, Kv} end, AllKv),
    WithConcat = lists:map(
        fun(Condition) ->
            "SET " ++ Actual = make_one_command(Condition),
            Actual
        end,

        NewKv
    ),
    WithFormatted = lists:flatten(lists:join(", ", WithConcat)),

    io_lib:format("SET ~s", [WithFormatted]);
%% LET Builders
make_one_command({var, {Key, Value}}) ->
    io_lib:format("LET $~s = ~p", [Key, Value]);
%% SLEEP Builders
make_one_command({sleep, MsDuration}) ->
    io_lib:format("SLEEP ~pms", [MsDuration]);
%% DELETE Builders
make_one_command({delete, Something}) ->
    io_lib:format("DELETE ~s", [Something]);
%% UPDATE Builders
make_one_command({update, Something}) ->
    io_lib:format("UPDATE ~s", [Something]);
%% USE Builders
make_one_command({use, {namespace, Namespace}}) ->
    io_lib:format("USE NS ~s", [Namespace]);
make_one_command({use, {database, Database}}) ->
    io_lib:format("USE DB ~s", [Database]);
make_one_command({use, {Namespace, Database}}) ->
    io_lib:format("USE NS ~s DB ~s", [Namespace, Database]);
make_one_command(_Other) ->
    "".
