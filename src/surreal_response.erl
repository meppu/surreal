%%% @doc Module for SurrealDB response types and internal functions about it.
-module(surreal_response).

-record(server_error, {
    code :: integer(),
    message :: string()
}).

-type server_error() :: #server_error{}.
-type response() :: list({ok, any()} | {error, string()}).
-type result() :: server_error() | response().

%%% For external usage.
-export_type([server_error/0, response/0, result/0]).

%%% For internal usage.
-export([to_response/1]).

%%% Private functions.
%%% -----------------

%% @hidden
to_response(#{<<"error">> := #{<<"code">> := Code, <<"message">> := ErrorMsg}}) ->
    #server_error{
        code = Code,
        message = ErrorMsg
    };
to_response(#{<<"time">> := _Time, <<"status">> := <<"ERR">>, <<"detail">> := ErrorMsg}) ->
    {error, ErrorMsg};
to_response(#{<<"time">> := _Time, <<"status">> := <<"OK">>, <<"result">> := Result}) ->
    {ok, Result};
to_response(#{<<"result">> := null}) ->
    [];
to_response(#{<<"result">> := Results}) when is_list(Results) ->
    lists:map(fun to_response/1, Results);
to_response(Other) ->
    {ok, Other}.
