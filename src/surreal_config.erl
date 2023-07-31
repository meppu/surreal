%%%-------------------------------------------------------------------------
%%% @copyright (C) 2023, meppu
%%% @doc Connection configuration module for SurrealDB driver.
%%%
%%% == SurrealDB URI Format ==
%%%
%%% The SurrealDB URI format is an unofficial way to represent connection information
%%% for accessing SurrealDB. It allows users to define various parameters necessary
%%% for establishing a connection to the database server. The URI format follows the format:
%%%
%%% - <strong>Plain TCP</strong>:
%%%   `surrealdb://username:password@host:port/namespace/database'
%%%
%%% - <strong>TLS</strong>:
%%%   `surrealdb+tls://username:password@host:port/namespace/database'
%%%
%%% === Scheme ===
%%%
%%%     This is the scheme used to identify SurrealDB connections. The optional "tls" part
%%%     indicates that the connection should be made over TLS (Transport Layer Security),
%%%     providing a secure and encrypted communication channel.
%%%
%%% === Credentials ===
%%%
%%%     The username and password to authenticate with the SurrealDB server.
%%%
%%% === Host ===
%%%
%%%     The hostname or IP address of the SurrealDB server.
%%%
%%% === Namespace ===
%%%
%%%     The name of the SurrealDB namespace that you want to use.
%%%
%%% === Database ===
%%%
%%%     The name of the SurrealDB database that you want to use.
%%%
%%% == Example URI ==
%%%
%%% <strong>Default for SurrealDB</strong>:
%%% `surrealdb://root:root@localhost:8000/test/test'
%%%
%%% @author meppu
%%% @end
%%%-------------------------------------------------------------------------
-module(surreal_config).

-export([parse/1]).
-export_type([connection_map/0]).

%%%==========================================================================
%%%
%%%   Public types
%%%
%%%==========================================================================

-type connection_map() :: #{
    host => string(),
    username => string(),
    password => string(),
    namespace => string(),
    database => string(),
    port => non_neg_integer(),
    tls => boolean()
}.

%%%==========================================================================
%%%
%%%   Public functions
%%%
%%%==========================================================================

%%-------------------------------------------------------------------------
%% @doc Parse SurrealDB URI.
%%
%% ```
%1> surreal_config:parse("surrealdb://root:root@localhost:8000/test/test").
%%  % {ok,#{database => "test",host => "localhost",namespace => "test",
%%  %       password => "root",path => "/test/test",port => 8000,
%%  %       tls => false,username => "root"}}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec parse(Uri :: nonempty_string()) -> {ok, connection_map()} | {error, atom(), term()}.
parse(Uri) ->
    case uri_string:parse(Uri) of
        #{
            host := Host,
            path := Path,
            port := Port,
            scheme := Scheme,
            userinfo := UserInfo
        } ->
            {ok, Tls} = parse_scheme(Scheme),
            {ok, [Username, Password]} = parse_userinfo(UserInfo),
            {ok, [Namespace, Database]} = parse_path(Path),

            {ok, #{
                host => Host,
                path => Path,
                username => Username,
                password => Password,
                namespace => Namespace,
                database => Database,
                port => Port,
                tls => Tls
            }};
        {error, _, _} = Error ->
            Error;
        _ ->
            {error, invalid_uri, []}
    end.

%%%==========================================================================
%%%
%%%   Private functions
%%%
%%%==========================================================================

%% @private
parse_userinfo(UserInfo) ->
    case string:split(UserInfo, ":", all) of
        [_, _] = Data -> {ok, Data};
        _ -> {error, invalid_userinfo}
    end.

%% @private
parse_path(Path) ->
    case string:split(Path, "/", all) of
        [[], Namespace, Database] -> {ok, [Namespace, Database]};
        _ -> {error, invalid_path}
    end.

%% @private
parse_scheme("surrealdb") ->
    {ok, false};
parse_scheme("surrealdb+tls") ->
    {ok, true};
parse_scheme(_Other) ->
    {error, invalid_scheme}.
