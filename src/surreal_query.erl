-module(surreal_query).

-export([make_one/1]).

make_one(Query) ->
    Results = lists:map(fun make_one_command/1, Query),
    WithFormatted = lists:join(" ", Results) ++ ";",

    lists:flatten(WithFormatted).

%% @private
make_one_command({select}) ->
    "SELECT *";
make_one_command({select, Field}) when is_binary(Field); is_atom(Field); is_bitstring(Field) ->
    io_lib:format("SELECT ~s", [Field]);
make_one_command({select, Fields}) when is_list(Fields) ->
    ToString = lists:map(fun atom_to_list/1, Fields),
    WithFormatted = lists:flatten(lists:join(", ", ToString)),

    io_lib:format("SELECT ~s", [WithFormatted]);
make_one_command({from, Target}) when is_binary(Target); is_atom(Target); is_bitstring(Target) ->
    io_lib:format("FROM ~s", [Target]);
make_one_command({from, Targets}) when is_list(Targets) ->
    ToString = lists:map(fun atom_to_list/1, Targets),
    WithFormatted = lists:flatten(lists:join(", ", ToString)),

    io_lib:format("FROM ~s", [WithFormatted]);
make_one_command({where, {Operator, Left, Right}}) ->
    io_lib:format("WHERE ~p ~s ~p", [Left, Operator, Right]);
make_one_command({where, {Left, Right}}) ->
    io_lib:format("WHERE ~p = ~p", [Left, Right]);
make_one_command({where, Conditions}) when is_list(Conditions) ->
    NewConditions = lists:map(fun(Condition) -> {where, Condition} end, Conditions),
    WithConcat = lists:map(
        fun(Condition) ->
            [_, _, _, _, _, _ | Actual] = make_one_command(Condition),
            Actual
        end,

        NewConditions
    ),
    WithFormatted = lists:flatten(lists:join(", ", WithConcat)),

    io_lib:format("WHERE ~s", [WithFormatted]);
make_one_command(_Other) ->
    "".
