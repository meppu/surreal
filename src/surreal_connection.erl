%%% @private
-module(surreal_connection).
-behaviour(gen_server).

-export([
    %%==============
    %% Client
    %%==============
    start_link/2,
    send_message/3,
    close/1,
    %%==============
    %% Server
    %%==============
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%%%==========================================================================
%%%
%%%   Client
%%%
%%%==========================================================================

-spec start_link(Config, ConnName) -> gen_server:start_ret() when
    Config :: surreal_config:connection_map(),
    ConnName :: atom().
start_link(Config, ConnName) ->
    gen_server:start_link({local, ConnName}, ?MODULE, [Config], []).

-spec send_message(Pid, Method, Params) -> surreal_result:result() when
    Pid :: gen_server:server_ref(),
    Method :: binary(),
    Params :: list(term()).
send_message(Pid, Method, Params) ->
    Payload = #{
        <<"id">> => base64:encode(crypto:strong_rand_bytes(10)),
        <<"method">> => Method,
        <<"params">> => Params
    },

    {ChildPid, MonitorReference} = spawn_monitor(fun() ->
        gen_server:cast(Pid, {send, Payload, self()}),

        receive
            Data -> exit({ok, Data})
        end
    end),

    receive
        {'DOWN', MonitorReference, process, ChildPid, {ok, Response}} ->
            {ok, Response}
    after 5000 ->
        exit(ChildPid, kill),
        {error, timeout}
    end.

-spec close(gen_server:server_ref()) -> ok.
close(Pid) ->
    gen_server:cast(Pid, close).

%%%==========================================================================
%%%
%%%   Server
%%%
%%%==========================================================================

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

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast(
    {send, #{<<"id">> := PacketId} = Packet, Client}, {ConnPid, StreamRef, Table} = State
) ->
    ets:insert(Table, {PacketId, Client}),

    PacketEncoded = jsx:encode(Packet),
    gun:ws_send(ConnPid, StreamRef, {text, PacketEncoded}),

    {noreply, State};
handle_cast(close, State) ->
    {stop, {shutdown, client}, State}.

handle_info({gun_ws, ConnPid, StreamRef, {text, Packet}}, {_ConnPid, _StreamRef, Table}) ->
    #{<<"id">> := PacketId} = PacketDecoded = jsx:decode(Packet),

    case ets:lookup(Table, PacketId) of
        [{PacketId, Pid} | _Other] ->
            ets:delete(Table, PacketId),
            Pid ! PacketDecoded;
        _ ->
            noop
    end,

    {noreply, {ConnPid, StreamRef, Table}}.

terminate({shutdown, client}, {ConnPid, _StreamRef, Table}) ->
    ets:delete(Table),
    gun:close(ConnPid).
