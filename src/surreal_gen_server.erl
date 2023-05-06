%%% @doc SurrealDB Client as GenServer.
%%%
%%% Better not use this GenServer directly but for some cases you may need to use it.
-module(surreal_gen_server).
-behaviour(gen_server).

-define(RANDOM, base64:encode(crypto:strong_rand_bytes(10))).

-export([init/1, handle_call/3]).
-export([handle_cast/2, handle_info/2, code_change/3]).

init([Url]) ->
    Result = surreal_prv_websocket:start_link(Url ++ "/rpc", self()),

    receive
        ok -> Result
    end.

%%% -----------------------------------------------
%%% Following handlers are for database management.
%%% -----------------------------------------------

handle_call({signin, User, Pass}, _From, Connection) ->
    Response = send_payload(Connection, <<"signin">>, [
        #{
            <<"user">> => User,
            <<"pass">> => Pass
        }
    ]),

    {reply, Response, Connection};
handle_call({signup, Namespace, Database, Scope, Email, Password}, _From, Connection) ->
    Response = send_payload(Connection, <<"signup">>, [
        #{
            <<"NS">> => Namespace,
            <<"DB">> => Database,
            <<"SC">> => Scope,
            <<"email">> => Email,
            <<"pass">> => Password
        }
    ]),

    {reply, Response, Connection};
handle_call({authenticate, Token}, _From, Connection) ->
    Response = send_payload(Connection, <<"authenticate">>, [Token]),
    {reply, Response, Connection};
handle_call({invalidate}, _From, Connection) ->
    Response = send_payload(Connection, <<"invalidate">>, []),
    {reply, Response, Connection};
handle_call({use, Namespace, Database}, _From, Connection) ->
    Response = send_payload(Connection, <<"use">>, [Namespace, Database]),
    {reply, Response, Connection};
%%% -----------------------------------------------
%%% Following handlers are for document management.
%%% -----------------------------------------------

handle_call({query, Query, Params}, _From, Connection) ->
    Response = send_payload(Connection, <<"query">>, [Query, Params]),
    {reply, Response, Connection};
handle_call({select, TableOrId}, _From, Connection) ->
    Response = send_payload(Connection, <<"select">>, [TableOrId]),
    {reply, Response, Connection};
handle_call({create, TableOrId, Data}, _From, Connection) ->
    Response = send_payload(Connection, <<"create">>, [TableOrId, Data]),
    {reply, Response, Connection};
handle_call({update, TableOrId, Data}, _From, Connection) ->
    Response = send_payload(Connection, <<"update">>, [TableOrId, Data]),
    {reply, Response, Connection};
handle_call({change, TableOrId, Data}, _From, Connection) ->
    Response = send_payload(Connection, <<"change">>, [TableOrId, Data]),
    {reply, Response, Connection};
handle_call({modify, TableOrId, Data}, _From, Connection) ->
    Response = send_payload(Connection, <<"modify">>, [TableOrId, Data]),
    {reply, Response, Connection};
handle_call({delete, TableOrId}, _From, Connection) ->
    Response = send_payload(Connection, <<"delete">>, [TableOrId]),
    {reply, Response, Connection}.

%%% -------------------------------------------
%%% Following functions are for internal usage.
%%% -------------------------------------------

%% @private
send_payload(Connection, Method, Params) ->
    Payload = #{
        <<"id">> => ?RANDOM,
        <<"method">> => Method,
        <<"params">> => Params
    },

    surreal_response:to_response(surreal_prv_websocket:send_message(Connection, Payload)).

%%% --------------------------------------------------
%%% Following handlers are for keeping compiler quite.
%%% --------------------------------------------------

%% @private
handle_cast({stop}, _Connection) ->
    {noreply, null}.

%% @private
handle_info(_Message, Connection) ->
    {noreply, Connection}.

%% @private
code_change(_OldVersion, Connection, _Extra) ->
    {ok, Connection}.
