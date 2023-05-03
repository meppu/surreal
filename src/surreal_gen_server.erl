%%% @doc SurrealDB Client as GenServer.
%%%
%%% Better not use this GenServer directly but for some cases you may need to use it.
-module(surreal_gen_server).
-behaviour(gen_server).

-define(RANDOM, base64:encode(crypto:strong_rand_bytes(10))).

%%% Mostly internal, but nothing stops you to use this GenServer.
-export([init/1, handle_call/3]).

%%% Keeps compiler quite.
-export([handle_cast/2, handle_info/2, code_change/3]).

init([Url]) ->
    surreal_prv_websocket:start_link(Url ++ "/rpc").

handle_call({signin, User, Pass}, _From, Connection) ->
    Payload = #{
        <<"id">> => ?RANDOM,
        <<"method">> => <<"signin">>,
        <<"params">> => [
            #{
                <<"user">> => User,
                <<"pass">> => Pass
            }
        ]
    },

    Response = surreal_response:to_response(
        surreal_prv_websocket:send_message(Connection, Payload)
    ),

    {reply, Response, Connection};
handle_call({use, Namespace, Database}, _From, Connection) ->
    Payload = #{
        <<"id">> => ?RANDOM,
        <<"method">> => <<"use">>,
        <<"params">> => [
            Namespace, Database
        ]
    },

    Response = surreal_response:to_response(
        surreal_prv_websocket:send_message(Connection, Payload)
    ),

    {reply, Response, Connection};
handle_call({query, Query, Params}, _From, Connection) ->
    Payload = #{
        <<"id">> => ?RANDOM,
        <<"method">> => <<"query">>,
        <<"params">> => [Query, Params]
    },

    Response = surreal_response:to_response(
        surreal_prv_websocket:send_message(Connection, Payload)
    ),

    {reply, Response, Connection};
handle_call({select, TableOrId}, _From, Connection) ->
    Payload = #{
        <<"id">> => ?RANDOM,
        <<"method">> => <<"select">>,
        <<"params">> => [TableOrId]
    },

    Response = surreal_response:to_response(
        surreal_prv_websocket:send_message(Connection, Payload)
    ),

    {reply, Response, Connection};
handle_call({create, TableOrId, Data}, _From, Connection) ->
    Payload = #{
        <<"id">> => ?RANDOM,
        <<"method">> => <<"create">>,
        <<"params">> => [TableOrId, Data]
    },

    Response = surreal_response:to_response(
        surreal_prv_websocket:send_message(Connection, Payload)
    ),

    {reply, Response, Connection};
handle_call({update, TableOrId, Data}, _From, Connection) ->
    Payload = #{
        <<"id">> => ?RANDOM,
        <<"method">> => <<"update">>,
        <<"params">> => [TableOrId, Data]
    },

    Response = surreal_response:to_response(
        surreal_prv_websocket:send_message(Connection, Payload)
    ),

    {reply, Response, Connection};
handle_call({change, TableOrId, Data}, _From, Connection) ->
    Payload = #{
        <<"id">> => ?RANDOM,
        <<"method">> => <<"change">>,
        <<"params">> => [TableOrId, Data]
    },

    Response = surreal_response:to_response(
        surreal_prv_websocket:send_message(Connection, Payload)
    ),

    {reply, Response, Connection};
handle_call({modify, TableOrId, Data}, _From, Connection) ->
    Payload = #{
        <<"id">> => ?RANDOM,
        <<"method">> => <<"modify">>,
        <<"params">> => [TableOrId, Data]
    },

    Response = surreal_response:to_response(
        surreal_prv_websocket:send_message(Connection, Payload)
    ),

    {reply, Response, Connection};
handle_call({delete, TableOrId}, _From, Connection) ->
    Payload = #{
        <<"id">> => ?RANDOM,
        <<"method">> => <<"delete">>,
        <<"params">> => [TableOrId]
    },

    Response = surreal_response:to_response(
        surreal_prv_websocket:send_message(Connection, Payload)
    ),

    {reply, Response, Connection}.

%% @hidden
handle_cast({stop}, _Connection) ->
    {noreply, null}.

%% @hidden
handle_info(_Message, Connection) ->
    {noreply, Connection}.

%% @hidden
code_change(_OldVersion, Connection, _Extra) ->
    {ok, Connection}.
