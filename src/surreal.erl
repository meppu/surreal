%%% @doc Erlang driver for SurrealDB.
-module(surreal).

-export([
    start_link/2,
    signin/3,
    use/3
]).

%%%-------------------------------------------------------------------------
%%% Private types
%%%-------------------------------------------------------------------------
-type surreal_pid() :: gen_server:server_ref().

%%%==========================================================================
%%%
%%%   Public functions
%%%
%%%==========================================================================

%% @doc Connects to a local or remote database endpoint.
-spec start_link(Url :: string(), ConnName :: atom()) -> gen_server:start_ret().
start_link(Url, ConnName) ->
    {ok,
        Config = #{
            username := Username,
            password := Password,
            namespace := Namespace,
            database := Database
        }} = surreal_config:parse(Url),

    case surreal_connection:start_link(Config, ConnName) of
        {ok, Pid} ->
            {ok, _} = signin(Pid, Username, Password),
            {ok, _} = use(Pid, Namespace, Database),

            {ok, Pid};
        Other ->
            Other
    end.

%% @doc Sign in to the database. This is a necessary step before using the database.
-spec signin(surreal_pid(), Username :: string(), Password :: string()) -> surreal_result:result().
signin(Pid, Username, Password) ->
    Params = [
        #{
            <<"user">> => unicode:characters_to_binary(Username),
            <<"pass">> => unicode:characters_to_binary(Password)
        }
    ],
    surreal_connection:send_message(Pid, <<"signin">>, Params).

%% @doc Switch to a specific namespace and database.
-spec use(surreal_pid(), Namespace :: string(), Database :: string()) -> surreal_result:result().
use(Pid, Namespace, Database) ->
    Params = [unicode:characters_to_binary(Namespace), unicode:characters_to_binary(Database)],
    surreal_connection:send_message(Pid, <<"use">>, Params).
