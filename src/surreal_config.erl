-module(surreal_config).

-export([parse/1]).
-export_type([connection_map/0, uri/0, ok/0, error/0]).

%%%-------------------------------------------------------------------------
%%% Public connection configuration type
%%%-------------------------------------------------------------------------
-type connection_map() :: #{
    host => unicode:chardata(),
    username => unicode:chardata(),
    password => unicode:chardata(),
    namespace => unicode:chardata(),
    database => unicode:chardata(),
    port => non_neg_integer(),
    tls => boolean()
}.

%%%-------------------------------------------------------------------------
%%% Public types for configuration input/output
%%%-------------------------------------------------------------------------
-type uri() :: iodata().

-type ok() :: {ok, connection_map()}.
-type error() :: {error, atom(), term()}.

%%%-------------------------------------------------------------------------
%%% Public functions
%%%-------------------------------------------------------------------------
-spec parse(uri()) -> ok() | error().
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

            #{
                host => Host,
                path => Path,
                username => Username,
                password => Password,
                namespace => Namespace,
                database => Database,
                port => Port,
                tls => Tls
            };
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
