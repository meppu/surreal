-module(surreal_config).

-export([parse/1]).
-export_type([connection_map/0]).

%%%-------------------------------------------------------------------------
%%% Public connection configuration type
%%%-------------------------------------------------------------------------
-type connection_map() :: #{
    host => string(),
    username => string(),
    password => string(),
    namespace => string(),
    database => string(),
    port => non_neg_integer(),
    tls => boolean()
}.

%%%-------------------------------------------------------------------------
%%% Public functions
%%%-------------------------------------------------------------------------
%% @doc Parse SurrealDB URI.
-spec parse(Uri :: iodata()) -> {ok, connection_map()} | {error, atom(), term()}.
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

%%%-------------------------------------------------------------------------
%%% Private functions
%%%-------------------------------------------------------------------------
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
parse_scheme(_) ->
    {error, invalid_scheme}.
