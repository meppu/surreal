-module(surreal_connection).
-behaviour(gen_server).

-export([
    %% Client
    %% ======
    start_link/2,
    send_message/4,
    %% Server
    %% ======
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%%==========================================================================
%%
%%   Client
%%
%%==========================================================================
-spec start_link(Config, ConnName) -> gen_server:start_ret() when
    Config :: surreal_config:connection_map(),
    ConnName :: atom().
start_link(Config, ConnName) ->
    gen_server:start_link({local, ConnName}, ?MODULE, [Config], []).

-spec send_message(Pid, Id, Method, Params) -> map() when
    Pid :: gen_server:server_ref(),
    Id :: binary(),
    Method :: binary(),
    Params :: list(term()).
send_message(Pid, Id, Method, Params) ->
    Payload = #{
        <<"id">> => Id,
        <<"method">> => Method,
        <<"params">> => Params
    },

    {ChildPid, MonitorReference} = spawn_monitor(fun() ->
        gen_server:call(Pid, {send, Payload}),

        receive
            Data -> exit({ok, Data})
        after 5000 ->
            exit({error, timeout})
        end
    end),

    receive
        {'DOWN', MonitorReference, process, ChildPid, {ok, Response} = Value} ->
            io:format("~p~n", [Response]),
            Value;
        {'DOWN', MonitorReference, process, ChildPid, Other} ->
            Other
    end.

%%==========================================================================
%%
%%   Server
%%
%%==========================================================================

%%-------------------------------------------------------------------------
%% Initialise callback for generic server behaviour
%%-------------------------------------------------------------------------
init([#{host := Host, port := Port, tls := Tls}]) ->
    % Ensure gun is running
    {ok, _List} = application:ensure_all_started(gun),

    Opts =
        case Tls of
            true -> #{transport => tls};
            false -> #{transport => tcp}
        end,

    {ok, ConnPid} = gun:open(Host, Port, Opts),

    % Wait for gun to up
    receive
        {gun_up, ConnPid, http} -> noop
    after 5000 ->
        exit({error, timeout})
    end,

    % Upgrade connection to WebSocket
    StreamRef = gun:ws_upgrade(ConnPid, "/rpc"),

    % Wait for it to establish
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _} ->
            Table = ets:new(t, [set, private]),
            {ok, {ConnPid, StreamRef, Table}};
        _Other ->
            {error, failed}
    after 5000 ->
        {error, timeout}
    end.

%%-------------------------------------------------------------------------
%% Handle call for server
%%-------------------------------------------------------------------------
handle_call(
    {send, #{<<"id">> := PacketId} = Packet}, {Client, _Tag}, {ConnPid, StreamRef, Table} = State
) ->
    ets:insert(Table, {PacketId, Client}),

    PacketEncoded = jsx:encode(Packet),
    gun:ws_send(ConnPid, StreamRef, {text, PacketEncoded}),

    {reply, PacketId, State};
handle_call(stop, _From, State) ->
    {stop, normal, State}.

%%-------------------------------------------------------------------------
%% Handle cast for server
%%-------------------------------------------------------------------------
handle_cast(_Message, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------------
%% Handle messages that directly sent to pid
%%-------------------------------------------------------------------------
handle_info({gun_ws, ConnPid, StreamRef, {text, Packet}}, {_ConnPid, _StreamRef, Table}) ->
    #{<<"id">> := PacketId} = PacketDecoded = jsx:decode(Packet),

    case ets:lookup(Table, PacketId) of
        [{PacketId, Pid} | _Other] ->
            Pid ! PacketDecoded;
        _ ->
            noop
    end,

    {noreply, {ConnPid, StreamRef, Table}}.

%%-------------------------------------------------------------------------
%% Handle terminate for server
%%-------------------------------------------------------------------------
terminate(normal, {ConnPid, _StreamRef, Table}) ->
    ets:delete(Table),
    gun:close(ConnPid).
