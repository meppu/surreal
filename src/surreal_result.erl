%%%-------------------------------------------------------------------------
%%% @copyright (C) 2023, meppu
%%% @doc Result types for SurrealDB driver.
%%%
%%% @author meppu
%%% @end
%%%-------------------------------------------------------------------------
-module(surreal_result).

-export([get_query_result/1, get_method_result/1]).
-export_type([server_error/0, query_error/0, ok/0, result/0]).

%%%==========================================================================
%%%
%%%   Public types
%%%
%%%==========================================================================

-type server_error() :: {error, Code :: integer(), Message :: iodata()}.
-type query_error() :: {error, atom() | iodata()}.
-type ok() :: {ok, term()}.

-type inner_result() :: server_error() | query_error() | ok().
-type result() :: inner_result() | list(inner_result()).

%%%==========================================================================
%%%
%%%   Undocumented public functions
%%%
%%%==========================================================================

%% @private
get_query_result(#{<<"error">> := #{<<"code">> := Code, <<"message">> := Message}}) ->
    {error, Code, Message};
get_query_result(#{<<"time">> := _Time, <<"status">> := <<"ERR">>, <<"detail">> := Message}) ->
    {error, Message};
get_query_result(#{<<"time">> := _Time, <<"status">> := <<"ERR">>, <<"result">> := Message}) ->
    {error, Message};
get_query_result(#{<<"time">> := _Time, <<"status">> := <<"OK">>, <<"result">> := Result}) ->
    {ok, Result};
get_query_result(List) when is_list(List) ->
    [get_query_result(I) || I <- List].

%% @private
get_method_result(#{<<"error">> := #{<<"code">> := Code, <<"message">> := Message}}) ->
    {error, Code, Message};
get_method_result(#{<<"result">> := Result}) ->
    {ok, Result};
get_method_result(Other) ->
    {ok, Other}.
