-module(surreal_ws).
-behaviour(websocket_client).

-export([
    %% What We Need
    start_link/2,
    send_message/2,
    %% Callbacks
    init/1,
    onconnect/2,
    ondisconnect/2,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
]).

%% @hidden
start_link(Url, Pid) ->
    websocket_client:start_link(Url, ?MODULE, [{pid, Pid}]).

%% @hidden
send_message(Pid, Text) ->
    websocket_client:send(Pid, {text, Text}).

%% Starting Callbacks
%% ------------------

%% @hidden
init([{pid, Pid}]) ->
    {once, Pid}.

%% @hidden
onconnect(_WSReq, State) ->
    {ok, State}.

%% @hidden
ondisconnect({remote, closed}, State) ->
    {reconnect, State}.

%% @hidden
websocket_handle({ping, <<>>}, _ConnState, State) ->
    {ok, State};
websocket_handle({text, Msg}, _ConnState, State) ->
    State ! Msg,
    {ok, State}.

%% @hidden
websocket_info(start, _ConnState, State) ->
    {ok, State}.

%% @hidden
websocket_terminate(Reason, _ConnState, State) ->
    io:format(
        "Websocket closed in state ~p wih reason ~p~n",
        [State, Reason]
    ),
    ok.
