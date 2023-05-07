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
make_one_command({from, Something}) ->
    io_lib:format("FROM ~s", [Something]);
make_one_command(_Other) ->
    "".
