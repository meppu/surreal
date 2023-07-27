-module(surreal_config).

-export([parse/1]).
-export_type([connection_map/0, uri/0, ok/0, error/0]).

%%-------------------------------------------------------------------------
%% Public connection configuration type
%%-------------------------------------------------------------------------
-type connection_map() :: #{
    host => unicode:chardata(),
    path => unicode:chardata(),
    port => non_neg_integer(),
    username => unicode:chardata(),
    password => unicode:chardata()
}.

%%-------------------------------------------------------------------------
%% Public types for configuration input/output
%%-------------------------------------------------------------------------
-type uri() :: iodata().

-type ok() :: {ok, connection_map()}.
-type error() :: {error, atom(), term()}.

%%-------------------------------------------------------------------------
%% Public functions
%%-------------------------------------------------------------------------
-spec parse(uri()) -> ok() | error().
parse(Uri) ->
    case uri_string:parse(Uri) of
        #{host := Host, path := Path, port := Port, scheme := _, userinfo := UserInfo} ->
            {ok, [Username, Password]} = parse_userinfo(UserInfo),
            #{host => Host, path => Path, port => Port, username => Username, password => Password};
        #{host := Host, path := Path, scheme := Scheme, userinfo := UserInfo} ->
            Port =
                case Scheme of
                    "https" -> 443;
                    "wss" -> 443;
                    _ -> 80
                end,

            {ok, [Username, Password]} = parse_userinfo(UserInfo),
            #{host => Host, path => Path, port => Port, username => Username, password => Password};
        {error, _, _} = Error ->
            Error;
        _ ->
            {error, invalid_uri, []}
    end.

%%-------------------------------------------------------------------------
%% Private functions
%%-------------------------------------------------------------------------
parse_userinfo(UserInfo) ->
    case string:split(UserInfo, ":", all) of
        [_, _] = Data -> {ok, Data};
        _ -> {error, invalid_userinfo}
    end.
