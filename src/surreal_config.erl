%%% @doc Module for advanced configs.
%%%
%%% This module allows you to set configs and use them with calling one function.
-module(surreal_config).

-export([load/1]).

%% @doc Start client from config.
-spec load(Config :: proplists:proplist()) ->
    {ok, pid()}.
load(Config) ->
    % This function needs a fix for readability.

    WebSocketSecure =
        case proplists:is_defined(secure, Config) of
            true ->
                "wss";
            false ->
                "ws"
        end,

    WebSocketUrl = lists:flatten(
        io_lib:format("~s://~s:~b", [
            WebSocketSecure, proplists:get_value(host, Config), proplists:get_value(port, Config)
        ])
    ),

    {ok, Pid} =
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
                    undefined -> surreal:start(WebSocketUrl);
                    Name -> gen_server:start({local, Name}, surreal_gen_server, [WebSocketUrl], [])
                end
        end,

    case proplists:get_value(signin, Config) of
        {User, Pass} ->
            surreal:signin(Pid, User, Pass);
        _ ->
            noop
    end,

    case proplists:get_value(use, Config) of
        {Namespace, Database} ->
            surreal:use(Pid, Namespace, Database);
        _ ->
            noop
    end,

    {ok, Pid}.
