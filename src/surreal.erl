-module(surreal).

-include("surreal.hrl").

-record(surreal_server_error, {
    code :: integer(),
    message :: string()
}).

-type surreal_server_error() :: #surreal_server_error{}.
-type surreal_response() :: list({ok, any()} | {error, string()}).

-export([start_link/1, query/3, query/2]).

-spec start_link(Url :: string()) -> {ok, pid()} | {error, term()}.
start_link(Url) ->
    surreal_ws:start_link(Url ++ "/rpc").

-spec query(Pid :: pid(), Query :: string(), Params :: map()) ->
    surreal_server_error() | surreal_response().
query(Pid, Query, Params) ->
    Payload = #{
        <<"id">> => ?RANDOM,
        <<"method">> => <<"query">>,
        <<"params">> => [list_to_bitstring(Query), Params]
    },

    to_response(surreal_ws:send_message(Pid, Payload)).

-spec query(Pid :: pid(), Query :: string()) ->
    surreal_server_error() | surreal_response().
query(Pid, Query) ->
    query(Pid, Query, #{}).

%% Internal Functions
%% ------------------
%% @hidden
to_response(#{<<"error">> := #{<<"code">> := Code, <<"message">> := ErrorMsg}}) ->
    #surreal_server_error{
        code = Code,
        message = ErrorMsg
    };
to_response(#{<<"result">> := null}) ->
    [];
to_response(#{<<"result">> := Results}) ->
    lists:map(fun to_response/1, Results);
to_response(#{<<"time">> := _Time, <<"status">> := <<"ERR">>, <<"detail">> := ErrorMsg}) ->
    {error, ErrorMsg};
to_response(#{<<"time">> := _Time, <<"status">> := <<"ERR">>, <<"result">> := Result}) ->
    {ok, Result}.
