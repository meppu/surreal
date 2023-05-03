%%% @doc Base module of the library.
-module(surreal).

%%% For external usages.
-export([
    start_link/1,
    signin/3,
    use/3,
    query/3, query/2,
    select/2,
    create/3,
    update/3,
    change/3,
    modify/3,
    delete/2
]).

%% @doc Connects to a local or remote database endpoint.
-spec start_link(Url :: string()) ->
    {ok, pid()} | {error, term()}.
start_link(Url) ->
    gen_server:start_link(surreal_gen_server, [Url], []).

%% @doc Signs this connection up to a specific authentication scope.
-spec signin(Connection :: pid(), User :: string(), Pass :: string()) ->
    surreal_response:result().
signin(Connection, User, Pass) ->
    gen_server:call(
        Connection,
        {signin, unicode:characters_to_binary(User), unicode:characters_to_binary(Pass)}
    ).

%% @doc Switch to a specific namespace and database.
-spec use(Connection :: pid(), Namespace :: string(), Database :: string()) ->
    surreal_response:result().
use(Connection, Namespace, Database) ->
    gen_server:call(
        Connection,
        {use, unicode:characters_to_binary(Namespace), unicode:characters_to_binary(Database)}
    ).

%% @doc Runs a set of SurrealQL statements against the database with parameters.
-spec query(Connection :: pid(), Query :: string(), Params :: map()) ->
    surreal_response:result().
query(Connection, Query, Params) ->
    gen_server:call(
        Connection,
        {query, unicode:characters_to_binary(Query), Params}
    ).

%% @doc Runs a set of SurrealQL statements against the database.
-spec query(Connection :: pid(), Query :: string()) ->
    surreal_response:result().
query(Connection, Query) ->
    query(Connection, Query, #{}).

%% @doc Selects all records in a table, or a specific record, from the database.
-spec select(Connection :: pid(), TableOrId :: string()) ->
    surreal_response:result().
select(Connection, TableOrId) ->
    gen_server:call(
        Connection,
        {select, unicode:characters_to_binary(TableOrId)}
    ).

%% @doc Creates a record in the database.
-spec create(Connection :: pid(), TableOrId :: string(), Data :: map()) ->
    surreal_response:result().
create(Connection, TableOrId, Data) ->
    gen_server:call(
        Connection,
        {create, unicode:characters_to_binary(TableOrId), Data}
    ).

%% @doc Updates all records in a table, or a specific record.
-spec update(Connection :: pid(), TableOrId :: string(), Data :: map()) ->
    surreal_response:result().
update(Connection, TableOrId, Data) ->
    gen_server:call(
        Connection,
        {update, unicode:characters_to_binary(TableOrId), Data}
    ).

%% @doc Change all records in a table, or a specific record.
-spec change(Connection :: pid(), TableOrId :: string(), Data :: map()) ->
    surreal_response:result().
change(Connection, TableOrId, Data) ->
    gen_server:call(
        Connection,
        {change, unicode:characters_to_binary(TableOrId), Data}
    ).

%% @doc Applies JSON Patch changes to all records in a table, or a specific record.
-spec modify(Connection :: pid(), TableOrId :: string(), Data :: map()) ->
    surreal_response:result().
modify(Connection, TableOrId, Data) ->
    gen_server:call(
        Connection,
        {modify, unicode:characters_to_binary(TableOrId), Data}
    ).

%% @doc Deletes all records in a table, or a specific record, from the database.
-spec delete(Connection :: pid(), TableOrId :: string()) ->
    surreal_response:result().
delete(Connection, TableOrId) ->
    gen_server:call(
        Connection,
        {delete, unicode:characters_to_binary(TableOrId)}
    ).
