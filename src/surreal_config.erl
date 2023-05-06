%%% @doc Module for advanced configs.
%%%
%%% This module allows you to set configs and use them with calling one function.
-module(surreal_config).

-type config_value() ::
    secure
    | link
    | {name, atom()}
    | {host, string()}
    | {port, integer()}
    | {signin, {string(), string()}}
    | {use, {string(), string()}}.

-type config() :: list(config_value()).

-export([load/1]).

%% @doc Start client from config.
-spec load(Config :: config()) ->
    {ok, pid()}.
load(Config) ->
    case load_piece({start, Config}) of
        {ok, Pid} ->
            load_piece({signin, Pid, Config}),
            load_piece({use, Pid, Config}),

            {ok, Pid};
        Other ->
            Other
    end.

%%% -------------------------------------------
%%% Following functions are for internal usage.
%%% -------------------------------------------

%% @private
load_piece({protocol, Config}) ->
    case proplists:is_defined(secure, Config) of
        true ->
            "wss";
        false ->
            "ws"
    end;
load_piece({url, Config}) ->
    WebSocketProtocol = load_piece({protocol, Config}),
    WebSocketHost = proplists:get_value(host, Config),
    WebSocketUrl =
        case proplists:get_value(port, Config) of
            undefined ->
                io_lib:format("~s://~s", [WebSocketProtocol, WebSocketHost]);
            Port ->
                io_lib:format("~s://~s:~b", [WebSocketProtocol, WebSocketHost, Port])
        end,

    lists:flatten(WebSocketUrl);
load_piece({start, Config}) ->
    WebSocketUrl = load_piece({url, Config}),

    case proplists:is_defined(link, Config) of
        true ->
            case proplists:get_value(name, Config) of
                undefined ->
                    surreal:start_link(WebSocketUrl);
                Name ->
                    gen_server:start_link({local, Name}, surreal_gen_server, [WebSocketUrl], [])
            end;
        false ->
            case proplists:get_value(name, Config) of
                undefined ->
                    surreal:start(WebSocketUrl);
                Name ->
                    gen_server:start({local, Name}, surreal_gen_server, [WebSocketUrl], [])
            end
    end;
load_piece({signin, Pid, Config}) ->
    case proplists:get_value(signin, Config) of
        {User, Pass} ->
            surreal:signin(Pid, User, Pass);
        _ ->
            noop
    end;
load_piece({use, Pid, Config}) ->
    case proplists:get_value(use, Config) of
        {Namespace, Database} ->
            surreal:use(Pid, Namespace, Database);
        _ ->
            noop
    end.
