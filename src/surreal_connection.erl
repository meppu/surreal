%%% @private
-module(surreal_connection).
-behaviour(gen_server).

-export([start_link/2, send_message/3, send_and_wait/2, close/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

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
    Params :: list(term()) | null.
send_message(Pid, Method, Params) ->
    #{timeout := Timeout} = gen_server:call(Pid, get_opts),

    Payload = #{
        <<"id">> => base64:encode(crypto:strong_rand_bytes(12)),
        <<"method">> => Method,
        <<"params">> => Params
    },

    RequestId = rpc:async_call(node(), ?MODULE, send_and_wait, [Pid, Payload]),
    case rpc:nb_yield(RequestId, Timeout) of
        {value, Data} ->
            {ok, Data};
        timeout ->
            {error, timeout}
    end.

-spec send_and_wait(Pid, Payload) -> term() when
    Pid :: gen_server:server_ref(),
    Payload :: list(term()) | null.
send_and_wait(Pid, Payload) ->
    gen_server:cast(Pid, {send, Payload, self()}),

    receive
        Data ->
            Data
    end.

-spec close(gen_server:server_ref()) -> ok.
close(Pid) ->
    gen_server:cast(Pid, close).

%%%==========================================================================
%%%
%%%   Server
%%%
%%%==========================================================================

init([#{host := Host, port := Port, tls := Tls, timeout := Timeout} = Opts]) ->
    % Ensure gun is running
    {ok, _List} = application:ensure_all_started(gun),

    GunOpts =
        case Tls of
            true -> #{transport => tls};
            false -> #{transport => tcp}
        end,

    {ok, ConnPid} = gun:open(Host, Port, GunOpts),

    % Wait for gun to up
    receive
        {gun_up, ConnPid, http} -> noop
    after Timeout ->
        exit({error, timeout})
    end,

    % Upgrade connection to WebSocket
    StreamRef = gun:ws_upgrade(ConnPid, "/rpc"),

    % Wait for it to establish
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _} ->
            Table = ets:new(t, [set, private]),
            {ok, {ConnPid, StreamRef, Table, Opts}};
        _Other ->
            {error, failed}
    after Timeout ->
        {error, timeout}
    end.

handle_call(get_opts, _From, {_ConnPid, _StreamRef, _Table, Opts} = State) ->
    {reply, Opts, State}.

handle_cast(
    {send, #{<<"id">> := PacketId} = Packet, Client}, {ConnPid, StreamRef, Table, _Opts} = State
) ->
    ets:insert(Table, {PacketId, Client}),

    PacketEncoded = jsx:encode(Packet),
    gun:ws_send(ConnPid, StreamRef, {text, PacketEncoded}),

    {noreply, State};
handle_cast(close, State) ->
    {stop, {shutdown, client}, State}.

handle_info({gun_ws, ConnPid, StreamRef, {text, Packet}}, {_ConnPid, _StreamRef, Table, Opts}) ->
    #{<<"id">> := PacketId} = PacketDecoded = jsx:decode(Packet),

    case ets:lookup(Table, PacketId) of
        [{PacketId, Pid} | _Other] ->
            ets:delete(Table, PacketId),
            Pid ! PacketDecoded;
        _Other ->
            noop
    end,

    {noreply, {ConnPid, StreamRef, Table, Opts}}.

terminate({shutdown, client}, {ConnPid, _StreamRef, Table, _Opts}) ->
    ets:delete(Table),
    gun:close(ConnPid).
