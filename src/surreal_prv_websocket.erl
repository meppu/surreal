%%% @hidden
-module(surreal_prv_websocket).
-behaviour(websocket_client).

-export([
    % What We Need
    start_link/1,
    send_message/2,
    message_broker/0,
    % Callbacks
    init/1,
    onconnect/2,
    websocket_info/3,
    websocket_handle/3,
    ondisconnect/2,
    websocket_terminate/3
]).

start_link(Url) ->
    websocket_client:start_link(Url, ?MODULE, [{pid, spawn(fun message_broker/0)}]).

send_message(Conn, #{<<"id">> := Id} = Msg) ->
    {Pid, MonitorReference} = spawn_monitor(fun() ->
        Encoded = jiffy:encode(Msg),
        websocket_client:send(Conn, {text, Encoded}),

        receive
            X ->
                exit({ok, X})
        end
    end),

    ets:insert(surreal_pool, {Id, Pid}),

    receive
        {'DOWN', MonitorReference, process, Pid, {ok, Result}} ->
            ets:delete(surreal_pool, Id),
            Result
    end.

message_broker() ->
    receive
        #{<<"id">> := Id} = Data ->
            case ets:lookup(surreal_pool, Id) of
                [{Id, Pid} | _Other] ->
                    Pid ! Data;
                _ ->
                    noop
            end;
        _ ->
            noop
    end,

    message_broker().

%%%% Starting callbacks.
%%%% ------------------

init([{pid, State}]) ->
    case ets:whereis(surreal_pool) of
        undefined -> ets:new(surreal_pool, [public, named_table, {decentralized_counters, true}]);
        _ -> noop
    end,

    {once, State}.

onconnect(_WSReq, State) ->
    {ok, State}.

websocket_info(start, _ConnState, State) ->
    {ok, State}.

websocket_handle({ping, <<>>}, _ConnState, State) ->
    {ok, State};
websocket_handle({text, Msg}, _ConnState, State) ->
    State ! jiffy:decode(Msg, [return_maps]),
    {ok, State}.

ondisconnect({remote, closed}, State) ->
    {reconnect, State}.

websocket_terminate(_Reason, _ConnState, _State) ->
    ok.
