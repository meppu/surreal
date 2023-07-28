-module(surreal_result).

-export([to_result/1]).
-export_type([server_error/0, query_error/0, ok/0, result/0]).

%%%-------------------------------------------------------------------------
%%% Public types
%%%-------------------------------------------------------------------------
-type server_error() :: {error, Code :: integer(), Message :: binary()}.
-type query_error() :: {error, atom() | binary()}.
-type ok() :: {ok, term()}.

-type result() :: server_error() | query_error() | ok().

%%%-------------------------------------------------------------------------
%%% Undocumented public functions
%%%-------------------------------------------------------------------------
%% @private
to_result(#{<<"error">> := #{<<"code">> := Code, <<"message">> := Message}}) ->
    {error, Code, Message};
to_result(#{<<"time">> := _Time, <<"status">> := <<"ERR">>, <<"detail">> := Message}) ->
    {error, Message};
to_result(#{<<"time">> := _Time, <<"status">> := <<"OK">>, <<"result">> := Result}) ->
    {ok, Result};
to_result(#{<<"result">> := Result})->
    {ok, Result};
to_result(Other) ->
    {ok, Other}.
