%%% @doc Base module of the library.
-module(surreal).

%%% For external usages.
-export([
    start_link/1,
    start/1,
    signin/3,
    signin/6,
    signup/6,
    authenticate/2,
    invalidate/1,
    set/3,
    unset/2,
    use/3,
    query/3, query/2,
    select/2,
    create/3,
    update/3,
    change/3,
    modify/3,
    delete/2,
    close/1
]).

%% @doc Connects to a local or remote database endpoint.
-spec start_link(Url :: string()) ->
    {ok, pid()} | {error, term()}.
start_link(Url) ->
    gen_server:start_link(surreal_gen_server, [Url], []).

%% @doc Same with "start_link" but without linking process.
-spec start(Url :: string()) ->
    {ok, pid()} | {error, term()}.
start(Url) ->
    gen_server:start(surreal_gen_server, [Url], []).

%%% ------------------------------------------------
%%% Following functions are for database management.
%%% ------------------------------------------------

%% @doc Signs in to a specific authentication scope.
-spec signin(Connection :: gen_server:server_ref(), Username :: string(), Password :: string()) ->
    surreal_response:result().
signin(Connection, Username, Password) ->
    gen_server:call(
        Connection,
        {signin, unicode:characters_to_binary(Username), unicode:characters_to_binary(Password)}
    ).

%% @doc Signs in to a specific authentication scope with more advanced inputs.
-spec signin(
    Connection :: gen_server:server_ref(),
    Namespace :: string(),
    Database :: string(),
    Scope :: string(),
    Username :: string(),
    Password :: string()
) ->
    surreal_response:result().
signin(Connection, Namespace, Database, Scope, Username, Password) ->
    gen_server:call(
        Connection,
        {signin, unicode:characters_to_binary(Namespace), unicode:characters_to_binary(Database),
            unicode:characters_to_binary(Scope), unicode:characters_to_binary(Username),
            unicode:characters_to_binary(Password)}
    ).

%% @doc Signs up to a specific authentication scope.
-spec signup(
    Connection :: gen_server:server_ref(),
    Namespace :: string(),
    Database :: string(),
    Scope :: string(),
    Username :: string(),
    Password :: string()
) ->
    surreal_response:result().
signup(Connection, Namespace, Database, Scope, Username, Password) ->
    gen_server:call(
        Connection,
        {signup, unicode:characters_to_binary(Namespace), unicode:characters_to_binary(Database),
            unicode:characters_to_binary(Scope), unicode:characters_to_binary(Username),
            unicode:characters_to_binary(Password)}
    ).

%% @doc Authenticates the current connection with a JWT token.
-spec authenticate(Connection :: gen_server:server_ref(), Token :: string()) ->
    surreal_response:result().
authenticate(Connection, Token) ->
    gen_server:call(Connection, {authenticate, unicode:characters_to_binary(Token)}).

%% @doc Invalidates the authentication for the current connection.
-spec invalidate(Connection :: gen_server:server_ref()) ->
    surreal_response:result().
invalidate(Connection) ->
    gen_server:call(Connection, {invalidate}).

%% @doc Assigns a value as a parameter for this connection.
-spec set(Connection :: gen_server:server_ref(), Name :: string(), Value :: term()) ->
    surreal_response:result().
set(Connection, Name, Value) ->
    gen_server:call(Connection, {set, unicode:characters_to_binary(Name), Value}).

%% @doc	Removes a parameter for this connection
-spec unset(Connection :: gen_server:server_ref(), Name :: string()) -> surreal_response:result().
unset(Connection, Name) ->
    gen_server:call(Connection, {unset, unicode:characters_to_binary(Name)}).

%% @doc Switch to a specific namespace and database.
-spec use(Connection :: gen_server:server_ref(), Namespace :: string(), Database :: string()) ->
    surreal_response:result().
use(Connection, Namespace, Database) ->
    gen_server:call(
        Connection,
        {use, unicode:characters_to_binary(Namespace), unicode:characters_to_binary(Database)}
    ).

%%% ------------------------------------------------
%%% Following functions are for document management.
%%% ------------------------------------------------

%% @doc Runs a set of SurrealQL statements against the database with parameters.
-spec query(Connection :: gen_server:server_ref(), Query :: string(), Params :: map()) ->
    surreal_response:result().
query(Connection, Query, Params) ->
    gen_server:call(
        Connection,
        {query, unicode:characters_to_binary(Query), Params}
    ).

%% @doc Runs a set of SurrealQL statements against the database.
-spec query(Connection :: gen_server:server_ref(), Query :: string()) ->
    surreal_response:result().
query(Connection, Query) ->
    query(Connection, Query, #{}).

%% @doc Selects all records in a table, or a specific record, from the database.
-spec select(Connection :: gen_server:server_ref(), TableOrId :: string()) ->
    surreal_response:result().
select(Connection, TableOrId) ->
    gen_server:call(
        Connection,
        {select, unicode:characters_to_binary(TableOrId)}
    ).

%% @doc Creates a record in the database.
-spec create(Connection :: gen_server:server_ref(), TableOrId :: string(), Data :: map()) ->
    surreal_response:result().
create(Connection, TableOrId, Data) ->
    gen_server:call(
        Connection,
        {create, unicode:characters_to_binary(TableOrId), Data}
    ).

%% @doc Updates all records in a table, or a specific record.
-spec update(Connection :: gen_server:server_ref(), TableOrId :: string(), Data :: map()) ->
    surreal_response:result().
update(Connection, TableOrId, Data) ->
    gen_server:call(
        Connection,
        {update, unicode:characters_to_binary(TableOrId), Data}
    ).

%% @doc Change all records in a table, or a specific record.
-spec change(Connection :: gen_server:server_ref(), TableOrId :: string(), Data :: map()) ->
    surreal_response:result().
change(Connection, TableOrId, Data) ->
    gen_server:call(
        Connection,
        {change, unicode:characters_to_binary(TableOrId), Data}
    ).

%% @doc Applies JSON Patch changes to all records in a table, or a specific record.
-spec modify(Connection :: gen_server:server_ref(), TableOrId :: string(), Data :: map()) ->
    surreal_response:result().
modify(Connection, TableOrId, Data) ->
    gen_server:call(
        Connection,
        {modify, unicode:characters_to_binary(TableOrId), Data}
    ).

%% @doc Deletes all records in a table, or a specific record, from the database.
-spec delete(Connection :: gen_server:server_ref(), TableOrId :: string()) ->
    surreal_response:result().
delete(Connection, TableOrId) ->
    gen_server:call(
        Connection,
        {delete, unicode:characters_to_binary(TableOrId)}
    ).

%% @doc Close connection.
-spec close(Connection :: gen_server:server_ref()) -> shutdown_ok.
close(Connection) ->
    gen_server:call(Connection, {stop}).
